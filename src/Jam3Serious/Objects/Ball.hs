module Jam3Serious.Objects.Ball where

import Data.Map qualified as M
import Data.Map.Monoidal.Strict qualified as MM
import Data.Monoid
import Jam3Serious.Objects.Camera
import Jam3Serious.Drawing
import Jam3Serious.Geometry
import Jam3Serious.Mail
import Jam3Serious.Objects.Court
import Jam3Serious.Objects.Rim (getRims)
import Jam3Serious.Prelude
import Jam3Serious.Collisions


getBall :: SF () (V3 Double)
getBall = proc _ -> do
  g <- global -< ()
  returnA -< fromMaybe (trace "no ball" 9999) $ os_pos =<< M.lookup Ball (g_everyone g)

data BallState = BallState
  { bs_pos :: !(V3 Double)
  , bs_vel :: !(V3 Double)
  }
  deriving Generic

instance ToObjState BallState where
  toObjState ps = ObjState
    { os_pos = Just $ bs_pos ps
    , os_collision = Just $ ballCapsule $ bs_pos ps
    }

data PickMeUp = PickMeUp
  deriving Show

data PickedUp = PickedUp
  deriving Show


ballGravity :: V3 Double
ballGravity = V3 0 0 (-10)

ballElasticity :: Double
ballElasticity = 0.8

shootHeight, passHeight :: Double
shootHeight = 2.4
passHeight = 1.7

drawBall :: ToScreen -> BallState -> Output
drawBall cam bs = mconcat
  [ drawCapsule cam
      (ballCapsule $ bs_pos bs)
      (V4 255 128 0 255)
      (DDDepth $ view _y $ bs_pos bs)
  ]


-- | Compute when the ball ought to bounce, and how its velocity ought to
-- reflect.
bounces :: SF BallState (Event (V3 Double -> V3 Double))
bounces = proc bs -> do
  let pos = bs_pos bs
      vel = bs_vel bs

  wallBounce
    <- foldMap
         (\r -> fmap (fmap Endo) $ rect3Bounce r)
         (fmap fst courtGeom)
    -< pos
  rims <- getRims -< ()

  returnA -< fmap appEndo $ asum
    [ wallBounce
    , maybeToEvent $ getFirst $ flip foldMap rims $ \rim ->
        case pointInCapsule rim (ballCapsule pos) && dot vel (rim - pos) > 0 of
          True -> pure $ Endo $ reflectAlong $ normalize $ rim - pos
          False -> mempty
    ]


-- | Move the ball according to gravity and its velocity.
kinematics :: V3 Double -> SF BallState BallState
kinematics vel0 = proc bs -> do
  pos0 <- keep -< bs_pos bs
  dvel <- integral -< ballGravity
  let vel = vel0 + dvel
  dpos <- integral -< vel

  returnA -<
    ( bs
        & #bs_pos .~ pos0 + dpos
        & #bs_vel .~ vel
    )

-- | Move the ball according to physics (kinematics + bouncing).
physics :: V3 Double -> SF BallState BallState
physics vel0 =
  switch
    (proc bs -> do
      bs' <- kinematics vel0 -< bs
      bounce <- bounces -< bs
      returnA -<
        ( bs'
        , fmap ((^* ballElasticity) . ($ bs_vel bs')) bounce
        ))
    physics


ballPhysics :: V3 Double -> ObjE BallState Name
ballPhysics vel0 = proc (oi, bs) -> do
  cam <- getCamera -< ()
  bs' <- physics vel0 -< bs
  pickup <- onMail @PickedUp -< oi
  g <- global -< ()
  returnA -<
    ( ( mempty
        { oo_output = drawBall cam bs'
        , oo_outbox = mconcat
            [ broadcastAt
                (has #_Player)
                PickMeUp
                (ballCapsule $ bs_pos bs)
                (g_everyone g)
            ]
        }
      , bs'
      )
    , fmap from pickup
    )


ballCarry :: Name -> ObjE BallState (V3 Double)
ballCarry who = proc (oi, bs) -> do
  cam <- getCamera -< ()
  action <- onMail @BallAction -< oi
  pos <- namedFriend who os_pos -< ()
  let bs' = bs & #bs_pos .~ fromMaybe 999 pos
  returnA -<
    ( ( mempty { oo_output = drawBall cam bs' }
      , bs'
      )
    , fmap (unAction . message) action
    )


ball :: Obj BallState
ball = go 0
  where
    go v = switch (ballPhysics v) $ \a -> switch (ballCarry a) go


data BallAction
  = ShootAt { unAction :: V3 Double }
  | PassTo { unAction :: V3 Double }
  deriving stock (Eq, Ord, Show)


broadcastAt
    :: Typeable a
    => (Name -> Bool)
    -> a
    -> Capsule Double
    -> Map Name ObjState
    -> MonoidalMap Name [Dynamic]
broadcastAt p a cap oss = MM.fromList $ do
  (who, os) <- M.toList oss
  guard $ p who
  cap' <- maybeToList $ os_collision os
  guard $ capsuleInCapsule cap cap'
  pure (who, pure $ toDyn a)


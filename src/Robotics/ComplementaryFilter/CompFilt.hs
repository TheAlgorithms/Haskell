module Robotics.ComplementaryFilter.CompFilt where

import Robotics.ComplementaryFilter.TestData

-- Utility functions to extract X, Y, Z components from 3D vector.
getX :: (a, a, a) -> a
getX (x,_,_) = x

getY :: (a, a, a) -> a
getY (_,y,_) = y

getZ :: (a, a, a) -> a
getZ (_,_,z) = z

-- Extract accel data from list of floats
getAccel :: (RealFloat a) => [a] -> (a, a, a)
getAccel [] = (0, 0, 0)
getAccel s = if length s >= 6
                then (s!!0, s!!1, s!!2)
                else (0, 0, 0)

-- Extract gyro data from a lsit of floats
getGyro :: (RealFloat a) => [a] -> (a, a, a)
getGyro s = if length s >= 6 
               then (s!!3, s!!4, s!!5)
               else (0, 0, 0)

-- Function to calculate tilt angle from accelerometer reading.
-- By default the tilt measurement is made around the Z axis.
accelTiltAngle :: (RealFloat a) => (a, a, a) -> a
accelTiltAngle (_, y, z) = (atan2 z y)*180.0/pi


-- Complementary filter, uses the scanl pattern.
compFilt :: (RealFloat a) => [a] -> [a] -> a -> a -> [a]
compFilt ωs θ_accs α δt = scanl (\θ (ω, θ_acc) -> α*(θ + ω*δt) + (1-α)*θ_acc) 
                                (head θ_accs)
                                (zip ωs θ_accs)

-- Calculate tilts
calcTilt :: (RealFloat a) => [(a, a, a)] -> [(a, a, a)] -> a -> a -> [a]
calcTilt accel gyro α δt = compFilt (map getX gyro) (map accelTiltAngle accel) α δt

main = do
    let accels = map getAccel testData
    let gyros  = map getGyro testData
    let tilts = calcTilt accels gyros 0.95 0.01
    print tilts
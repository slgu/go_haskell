module Geometry
    (sphere_volume
    , sphere_area
    , cube_volume
    , cube_area
    , cuboid_area
    , cuboid_volume
    ) where
        sphere_volume :: Float -> Float
        sphere_volume r = r^3 * pi * 4 / 3
        sphere_area :: Float -> Float
        sphere_area r = r^2 * pi * 4
        cube_volume :: Float -> Float
        cube_volume x = x^3
        cube_area :: Float -> Float
        cube_area x = x^2 * 6
        cuboid_area :: Float -> Float -> Float -> Float
        cuboid_area x y z = 2 * x * y + 2 * x * z + 2 * y * z
        cuboid_volume :: Float -> Float -> Float -> Float
        cuboid_volume x y z = x * y * z

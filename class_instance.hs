data TrafficLight = Red | Yellow | Green


instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = True


instance Show TrafficLight where
    show Red = "red light"
    show Yellow = "yellow light"
    show Green = "green light"

--class constraint with instance
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

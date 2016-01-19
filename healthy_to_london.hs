main = do
    contents <- getContents
    let init_edge_weight = reverse [read x::Int |x <- lines contents]
    print $ calculate_shortest_path init_edge_weight

calculate_shortest_path weight = min (calculate_shortest_path_first weight) (calculate_shortest_path_second weight)
calculate_shortest_path_first (x:y:z:w:weight) =
    min (calculate_shortest_path_first (w:weight) + z) (calculate_shortest_path_second (w:weight) + x + y)
calculate_shortest_path_first (x:y:z:[]) = min z (x + y)
calculate_shortest_path_first _ = 0
calculate_shortest_path_second (x:y:z:w:weight) =
    min (calculate_shortest_path_second (w:weight) + y) (calculate_shortest_path_first (w:weight) + x + z)
calculate_shortest_path_second (x:y:z:[]) = min y (x + z)
calculate_shortest_path_second _ = 0

type Module = (String, Integer, String, [String])

sum' [] = 0
sum' (x:xs) = x + (sum' xs)

count_etcs = sum' [x | (_,x,_,_) <- list]

filter_by_cat :: String -> [Module] -> [Module]
filter_by_cat category list = [ m | m <- list, is_of_cat category m]

is_of_cat :: String -> Module -> Bool
is_of_cat category modul = category' == category
                           where
                              (_,_,category',_) = modul
                              
get_name :: Module -> String
get_name (name,_,_,_) = name

module_not_in_list :: [String] -> Module -> Bool
module_not_in_list [] _ = True
module_not_in_list (m:ms) search
  | m == (get_name search) = False
  | otherwise = module_not_in_list ms search

list = [("jpa",2,"sprachen",[]),
  ("jap2",2,"sprachen",["jap1"]),
  ("wed1", 4, "grundlagen", []),
  ("wed2", 4, "grundlagen", ["wed1"]),
  ("wed3", 4, "grundlagen", ["wed2"]),
  ("anl1", 4, "mathe", []),
  ("epj", 4, "aufbau", ["wed1", "anl1"])]

minCategoryPoints = [("grundlagen", 8),
  ("sprachen", 2),
  ("mathe", 4),
  ("aufbau",4)]

can_fullfil_this_category :: ([Module], Integer) -> Bool
can_fullfil_this_category (modules, minimal_required_etcs) = sum [x | (_,x,_,_) <- modules] >= minimal_required_etcs

enough_etcs_remaining :: [Module] -> [(String, Integer)] -> Bool
enough_etcs_remaining modules minimal_required = all can_fullfil_this_category [((filter_by_cat cat modules), min_points) | (cat,min_points) <-minimal_required]

can_still_graduate failed_modules = enough_etcs_remaining modules minCategoryPoints
  where
    modules = [m | m <- list, module_not_in_list failed_modules m]

assert expected asserted
    | expected == asserted = "Test successful"
    | otherwise = " Should be '" ++ show asserted ++ "' but is '" ++ show expected ++ "'"

test text expected asserted = do
  putStrLn (text ++ ": " ++ assert expected asserted)

should_fail_when_failed_to_many_grundlagen = can_still_graduate ["wed1", "wed2"]

should_pass_if_enough_modules_remaining = can_fullfil_this_category ([
  ("jpa",2,"sprachen",[]),
  ("jap2",2,"sprachen",["jap1"])
  ], 2)

should_fail_if_not_enough_modules_remaining = can_fullfil_this_category ([
  ("wed1",4,"grundlagen",[])
  ], 8)

should_pass_when_enough_modules_in_each_category = enough_etcs_remaining [("jpa",2,"sprachen",[]),
  ("jap2",2,"sprachen",["jap1"]),
  ("wed1", 4, "grundlagen", []),
  ("wed2", 4, "grundlagen", ["wed1"]),
  ("wed3", 4, "grundlagen", ["wed2"]),
  ("anl1", 4, "mathe", [])] [("grundlagen", 8),
    ("sprachen", 2),
    ("mathe", 4)]


should_fail_when_not_enough_modules_in_each_category = enough_etcs_remaining [("jpa",2,"sprachen",[]),
    ("jap2",2,"sprachen",["jap1"]),
    ("wed1", 4, "grundlagen", []),
    ("anl1", 4, "mathe", [])] [("grundlagen", 8),
      ("sprachen", 2),
      ("mathe", 8)]

main = do
  test "should_fail_when_failed_to_many_grundlagen" should_fail_when_failed_to_many_grundlagen False
  test "should_pass_if_enough_modules_remaining" should_pass_if_enough_modules_remaining True
  test "should_fail_if_not_enough_modules_remaining" should_fail_if_not_enough_modules_remaining False
  test "should_pass_when_enough_modules_in_each_category" should_pass_when_enough_modules_in_each_category True
  test "should_fail_when_not_enough_modules_in_each_category" should_fail_when_not_enough_modules_in_each_category False

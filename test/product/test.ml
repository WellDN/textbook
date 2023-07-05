open OUnit2
open Product 

let tests = "test suite for product" >::: [
    "empty" >:: (fun _ -> assert_equal 1 (product [1]));
    "two_elements" >:: (fun _ -> assert_equal 2 (product [2]));
    "three_elements" >:: (fun _ -> assert_equal 3 (product [3]));
]

let _ = run_test_tt_main tests

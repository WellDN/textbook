open OUnit2
open List_max 

let tests = "suite" >::: [
  "empty" >:: (fun _ -> assert_raises (Failure "list_max") (fun () -> list_max []));
  "nonempty" >:: (fun _ -> assert_equal 8 (list_max [3; 1; 4; 8]))
]

let _ = run_test_tt_main tests

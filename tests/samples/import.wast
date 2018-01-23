(module
  (type $func_i32 (func (param i32)))
  (type $func_i64 (func (param i64)))
  (type $func_f32 (func (param f32)))
  (type $func_f64 (func (param f64)))

  (import "spectest" "print" (func (param i32)))
  ;; JavaScript can't handle i64 yet.
  ;; (func (import "spectest" "print") (param i64))
  (import "spectest" "print" (func $print_i32 (param i32)))
  ;; JavaScript can't handle i64 yet.
  ;; (import "spectest" "print" (func $print_i64 (param i64)))
  (import "spectest" "print" (func $print_f32 (param f32)))
  (import "spectest" "print" (func $print_f64 (param f64)))
  (import "spectest" "print" (func $print_i32_f32 (param i32 f32)))
  (import "spectest" "print" (func $print_f64_f64 (param f64 f64)))
  (func $print_i32-2 (import "spectest" "print") (param i32))
  (func $print_f64-2 (import "spectest" "print") (param f64))
  (import "test" "func-i64->i64" (func $i64->i64 (param i64) (result i64)))

  (func (export "p1") (import "spectest" "print") (param i32))
  (func $p (export "p2") (import "spectest" "print") (param i32))
  (func (export "p3") (export "p4") (import "spectest" "print") (param i32))
  (func (export "p5") (import "spectest" "print") (type 0))
  (func (export "p6") (import "spectest" "print") (type 0) (param i32) (result))

  (import "spectest" "print" (func (type $forward)))
  (func (import "spectest" "print") (type $forward))
  (type $forward (func (param i32)))

  (table anyfunc (elem $print_i32 $print_f64))
  (table (export "my-table") anyfunc (elem $print_i32 $print_f64))
  (table (export "my-table") (export "my-table2") anyfunc (elem $print_i32 $print_f64))
  (table (import "external-mod" "external-table") 0 anyfunc)
  (table (export "my-table") (import "external-mod" "external-table") 0 anyfunc)
  (table (export "my-table") (export "my-table2") (import "external-mod" "external-table") 0 anyfunc)

  (func (export "print32") (param $i i32)
    (local $x f32)
    (set_local $x (f32.convert_s/i32 (get_local $i)))
    (call 0 (get_local $i))
    (call $print_i32_f32
      (i32.add (get_local $i) (i32.const 1))
      (f32.const 42)
    )
    (call $print_i32 (get_local $i))
    (call $print_i32-2 (get_local $i))
    (call $print_f32 (get_local $x))
    (call_indirect (type $func_i32) (get_local $i) (i32.const 0))
  )

  (func (export "print64") (param $i i64)
    (local $x f64)
    (set_local $x (f64.convert_s/i64 (call $i64->i64 (get_local $i))))
    ;; JavaScript can't handle i64 yet.
    ;; (call 1 (get_local $i))
    (call $print_f64_f64
      (f64.add (get_local $x) (f64.const 1))
      (f64.const 53)
    )
    ;; JavaScript can't handle i64 yet.
    ;; (call $print_i64 (get_local $i))
    (call $print_f64 (get_local $x))
    (call $print_f64-2 (get_local $x))
    (call_indirect (type $func_f64) (get_local $x) (i32.const 1))
  )
)
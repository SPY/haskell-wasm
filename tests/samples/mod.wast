(module
  ;; Auxiliary definition
  (type $sig (func))
  (func $dummy)

  ;; Syntax

  (func)
  (func (import "extern_mod" "external_func") (param i32 i32) (result i64))
  (func (import "extern_mod" "external_func") (type $sig) (param i32 i32) (result i64))
  (func (import "extern_mod" "external_func") (type $sig))
  (func $add (param $x i32) (param $y i32) (result i32) (i32.add (i32.const 20) (i32.const 22)))
  (func (export "f"))
  (func $f)
  (func $h (export "g"))

  (func (local))
  (func (local) (local))
  (func (local i32))
  (func (local $x i32))
  (func (local i32 f64 i64))
  (func (local i32) (local f64))
  (func (local i32 f32) (local $x i64) (local) (local i32 f64))

  (func (param))
  (func (param) (param))
  (func (param i32))
  (func (param $x i32))
  (func (param i32 f64 i64))
  (func (param i32) (param f64))
  (func (param i32 f32) (param $x i64) (param) (param i32 f64))

  (func (result i32) (unreachable))
)

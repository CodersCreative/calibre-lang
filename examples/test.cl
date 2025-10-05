let mut a = 1;

print("The num is " & fn(num : &mut int) -> int => {
  *num = 10;
  let hello = *num
  print(num);
  return -10;
}(&mut a))
print(a)
a = 2;
print(a)

print(a + fn () => {a += 1 }())

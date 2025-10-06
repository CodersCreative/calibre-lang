let mut a = 1;

let mut b : <str, int>= ( "Ty", 9);

let mut c : list<int> = [10, 9, 32, 2];

print("b " & b[1])
print("c " & c[1])

print("The num is " & fn(num : &mut int) -> int => {
  *num = 10;
  let hello = *num
  print(*num);
  return -10;
}(&mut b[1]))

for i in &mut c => {
  *i += 5;
}

print("b " & b[1])
print("c " & c[1])
print(a)
a = 2;
print(a)

print(a + fn () => {a += 1 }())

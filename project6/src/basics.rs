/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
  if n<0 {return -1}
  let mut sum = 0;
  for k in 1..(n+1){
      sum+=k
  }
  sum
}

/**
    Returns the number of elements in the list that
    are in the range [s,e]
**/
pub fn in_range(lst: &[i32], s: i32, e: i32) -> i32 {
    let mut sum = 0;
    for i in lst.iter(){
        if i <= &e && i >= &s{
            sum+=1
        }
    }
    sum
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for i in target.iter(){
        if !set.contains(i){
            return false
        }
    }
    true
}

/**
    Returns the mean of elements in lst. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(lst: &[f64]) -> Option<f64> {
    if lst.len() == 0 {return None}
   let mut sum: f64 = 0.0;
    for i in lst.iter(){
            sum+=i
        }
   let size: f64 = lst.len() as f64;
   Some(sum / size)
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array

    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(lst: &[i32]) -> i32 {
    let mut sum = 0;
    for i in 1..lst.len()+1{
      sum+= &lst[lst.len()-i]*2_i32.pow((i-1) as u32) as i32
    }
    sum
}
/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut factors: Vec<u32> = Vec::new();
    let mut factors2: Vec<u32> = Vec::new();
    factors.push(n);

    while let Some(p) = factors.pop(){
      if p == 2{
        factors2.push(p);
      }
      else{
        for i in 2..p{
            if p%i == 0{
                factors.push(i);
                factors.push(p/i);
                break;
            }
            if i == p-1{
                factors2.push(p);
            }
        }
      }
    }

    factors2.sort();
    factors2
}

/**
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them,
    so the first becomes the last, the second becomes first, and so on.

    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut rot: Vec<i32> = Vec::new();
    if lst.len() == 0 {return rot;}
    for i in 1..lst.len(){
        rot.push(lst[i]);
    }
    rot.push(lst[0]);
    rot
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation

    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let targlen = target.len();
    if s.len() < targlen {return false;}
    for i in 0..s.len()-targlen+1 {
        if &s[i..i+targlen] == target{
            return true
        }
    }
    false
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
  if s.len() == 0 {return None}
    let mut sum = 1;
    let mut max = 0;
    let mut maxe = 0;
    for i in 1..s.len(){
        if &s[i..i+1] == &s[i-1..i]{
            sum+=1;
        }
        else {
            sum = 1;
        }
        if sum > max{
            max = sum;
            maxe = i;
        }
    }
    let q = maxe+1 - max;
    Some(&s[q..maxe+1])
}

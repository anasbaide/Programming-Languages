/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n >= 0 {
        let mut sum = 0;
        for i in 1..=n{
            sum = sum + i;
        }
        return sum;
    } else {
        return -1;
    }
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut num_elem = 0;
    for num in ls {
        if num >= &s && num <= &e {
            num_elem = num_elem + 1;
        }
    }
    return num_elem;
}


/**
    Returns true if target is a subset of set, false otherwise
    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for tar_elem in target {
        if set.contains(tar_elem) == false {
            return false;
        }
    }
    return true;
}
/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    let len = ls.len() as f64;
    if len == 0 as f64 {
        return None;
    } else {
        let mut sum = 0.0;
        for elem in ls{
            sum += elem;
        }
        return Some (sum as f64/len as f64);
    }
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    ls.iter().fold(0, |acc, &bit| acc*2 + bit)
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2
    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut num = n;
    let mut i = 2;
    let mut vec = Vec::new();

    while num > 1 {
        if num % i == 0 {
            vec.push(i);
            num = num / i;
        } else {
            i = i + 1;
        }
    }
    return vec;
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut vec = Vec::new();
    let len = lst.len();
    
    if len == 0 {
        return vec;
    } else {
        if len > 1 {
            for i in 1..len {
                vec.push(lst[i]);
            }
        }
    }

    vec.push(lst[0]);
    return vec;
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let s_len = s.len();
    let tar_len = target.len();

    if tar_len == 0 {
        return true;
    } else if (s_len == 0) && (tar_len > 0) {
        return false;
    } else {
        for i in 0..s_len {
            if (s_len - i) < tar_len {
                return false;
            } else if &s[i..i+tar_len] == target {
                return true;
            } 
        }
    }
    return false;
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters
    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    
    if s == "" {
        return None;
    } else { 
        let mut tokens:Vec<&str> = s.split("").collect();
        let mut temp_counter = 0;
        let mut counter = 0;
        let mut index_s = 0;
        let mut index_e = 0;
        tokens.remove(0);
        tokens.remove(tokens.len() - 1);

        for i in 0..(tokens.len()-1){
            if tokens[i] == tokens[i+1]{
                temp_counter += 1;
            } else if temp_counter > counter{
                counter = temp_counter;
                temp_counter = 0;
                index_s = i - counter;
                index_e = i;
            }
        }
        return Some(&s[index_s..(index_e + 1)]);
    }
}
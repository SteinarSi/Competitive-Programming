use std::io::{self, BufRead};


fn main(){
    let std = io::stdin();

    for line in std.lock().lines().map(|l| l.unwrap()){
        let nums: Vec<i64> = line.split(" ")
            .map(|num| num.parse().unwrap())
            .collect();
        let a = nums[0];
        let b = nums[1];

        println!("{}", (a-b).abs());
    }
}
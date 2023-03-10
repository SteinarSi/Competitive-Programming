use std::io;

fn main(){
    let mut input = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut input);

    let mut vec: Vec<u32> = input.trim().split(" ").map(|n| n.parse::<u32>().unwrap()).collect();
    vec.sort();
    for n in vec.iter(){
        print!("{} ", n);
    }
}
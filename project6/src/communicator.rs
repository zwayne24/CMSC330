#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method

    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        let mut st;
        match self {
            Command::Power(true, a) => {
                let s = String::from("Power increased by ");
                let m = String::from("%");
                st = s+&a.to_string()+&m;
            },
            Command::Power(false,a) => {
                let s = String::from("Power decreased by ");
                let n = String::from("%");
                st = s+&a.to_string()+&n;
            },
            Command::Missiles(true,a) => {
                let s = String::from("Missiles increased by ");
                st = s+&a.to_string();
            },
            Command::Missiles(false,a) => {
                let s = String::from("Missiles decreased by ");
                st = s+&a.to_string();
            },
            Command::Shield(w) => {
                let s = String::from("Shield turned ");
                let l = if *w {String::from("on")} else {String::from("off")};
                st = s+&l;
            },
            Command::Try => {
                st = String::from("Call attempt failed");
            },
            Command::Invalid => {
                st = String::from("Not a command");
            },
        };

        return st;
    }
}

/**
    Complete this method that converts a string to a command
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    let v: Vec<&str> = s.split(' ').collect();
    let mut x = Command::Invalid;
    if v.len() == 3 && v[0] == "power"{
        let b;
        match v[2].parse::<i32>(){
        Ok(_) => b = true,
        Err(_) => b = false,
        };
        if v[1] ==  "dec" && b {
            x = Command::Power(false,v[2].parse::<i32>().unwrap());
        }
        else if v[1] ==  "inc"&& b{
            x = Command::Power(true,v[2].parse::<i32>().unwrap());
        }
    }
    else if v.len() == 3 && v[0] == "add"{
        let c;
        match v[1].parse::<i32>(){
        Ok(_) => c = true,
        Err(_) => c = false,
        };
        if v.len() == 3 && v[2] == "missiles" && c {
            x = Command::Missiles(true,v[1].parse::<i32>().unwrap());
        }
    }
    else if v.len() == 3 && v[0] == "fire"{
        let d;
        match v[1].parse::<i32>(){
        Ok(_) => d = true,
        Err(_) => d = false,
        };
        if v.len() == 3 && v[2] == "missiles" && d {
            x = Command::Missiles(false,v[1].parse::<i32>().unwrap());
        }
    }
    else if v.len() == 2 && v[0] == "shield"{
        if v.len() == 2 && v[1] ==  "on" {
            x = Command::Shield(true);
        }
        else if  v.len() == 2 && v[1] ==  "off"{
            x = Command::Shield(false);
        }
    }
    else if v[0] == "try"{
        if v.len() > 4 {

        }
        else if v.len() == 4 && v[0] == "try" && v[1] == "calling" && v[2] == "Miss" && v[3] == "Potts"{
            x = Command::Try;
        }
    }
    return x
}

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
        match self {
            Command::Power(true, value) => {
                    let mut ret_val = String::from("Power increased by ");
                    ret_val.push_str(&value.to_string());
                    ret_val.push_str("%");
                    return ret_val;
            },
            Command::Power(false, value) => {
                    let mut ret_val = String::from("Power decreased by ");
                    ret_val.push_str(&value.to_string());
                    ret_val.push_str("%");
                    return ret_val;
            },
            Command::Missiles(true, value) => {
                    let mut ret_val = String::from("Missiles increased by ");
                    ret_val.push_str(&value.to_string());
                    return ret_val;
            },
            Command::Missiles(false, value) => {
                    let mut ret_val = String::from("Missiles decreased by ");
                    ret_val.push_str(&value.to_string());
                    return ret_val;
            },
            Command::Shield(true) => return String::from("Shield turned on"),
            Command::Shield(false) => return String::from("Shield turned off"),
            Command::Try => return String::from("Call attempt failed"),
            Command::Invalid => return String::from("Not a command")
        }
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
pub fn try_fn(s: &str) -> Command {
    let mut try_s = s.strip_prefix("try calling Miss Potts");
    match try_s {
        None => return Command::Invalid,
        Some(left_str) => { 
            if left_str == "" {
                return Command::Try;
            } else {
                return Command::Invalid;
            }
        }
    }
}

pub fn shieldoff_fn(s: &str) -> Command {
    let mut shield = s.strip_prefix("shield off");
    match shield {
        None => try_fn(s),
        Some(left_str) => { 
            if left_str == "" {
                return Command::Shield(false);
            } else {
                return Command::Invalid;
            }
        }
    }
}

pub fn shieldon_fn(s: &str) -> Command {
    let mut shield = s.strip_prefix("shield on");
    match shield {
        None => shieldoff_fn(s),
        Some(left_str) => { 
            if left_str == "" {
                return Command::Shield(true);
            } else {
                return Command::Invalid;
            }
        }
    }
}

pub fn miss_fire_fn(s: &str) -> Command {
    let mut miss_fire = s.strip_prefix("fire ");
    match miss_fire {
        None => shieldon_fn(s),
        Some(rest_str) => { 
            let tokens:Vec<&str> = rest_str.split_whitespace().collect();
            if (tokens[1] == "missiles") && (tokens.len() == 2) {
                let pow_int = tokens[0].parse::<i32>();
                match pow_int {
                    Ok(ok) => return Command::Missiles(false, ok),
                    Err(e) => return Command::Invalid, 
                } 
            } else {
                return Command::Invalid;
            } 
        }
    }
}

pub fn miss_add_fn(s: &str) -> Command {
    let mut miss_add = s.strip_prefix("add ");
    match miss_add {
        None => miss_fire_fn(s),
        Some(rest_str) => { 
            let tokens:Vec<&str> = rest_str.split_whitespace().collect();
            if (tokens[1] == "missiles") && (tokens.len() == 2) {
                let pow_int = tokens[0].parse::<i32>();
                match pow_int {
                    Ok(ok) => return Command::Missiles(true, ok),
                    Err(e) => return Command::Invalid, 
                } 
            } else {
                return Command::Invalid;
            } 
        }
    }
}

pub fn powe_dec_fn(s: &str) -> Command {
    let mut power_dec = s.strip_prefix("power dec ");
    match power_dec {
        None => miss_add_fn(s),
        Some(int_str) => { 
            let pow_int = int_str.parse::<i32>();
            match pow_int {
                Ok(ok) => return Command::Power(false, ok),
                Err(e) => return Command::Invalid, 
            }  
        }
    }
}

pub fn to_command(s: &str) -> Command {
    let mut power_inc = s.strip_prefix("power inc ");
    match power_inc {
        None => return powe_dec_fn(s),
        Some(int_str) => { 
            let pow_int = int_str.parse::<i32>();
            match pow_int {
                Ok(ok) => return Command::Power(true, ok),
                Err(e) => return Command::Invalid, 
            }  
        }
    }
}

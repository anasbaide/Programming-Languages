use std::{
    borrow::BorrowMut,
    ops::{Deref, DerefMut},
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Component {
    Helmet(bool),              //is damaged?
    LeftThrusters(bool, i32),  //is damaged? How much power left?
    RightThrusters(bool, i32), //is damaged? How much power left?
    LeftRepulsor(bool, i32),   //is damaged? How much power left?
    RightRepulsor(bool, i32),  //is damaged? How much power left?
    ChestPiece(bool, i32),     //is damaged? How much power left?
    Missiles(i32),             //how many missiles left?
    ArcReactor(i32),           // How much power left?
    Wifi(bool),                // connected to wifi?
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Armor {
    pub component: Component,
    pub version: i32,
}

// Part 2

// Students should fill in the Link type themselves. The Node and List types are given as is.
type Link = Option <Arc<RwLock<Node>>>;

struct Node {
    data: Armor,
    rest: Link,
}

#[derive(Clone)]
pub struct List {
    head_link: Link,
    size: usize,
}

impl List {

    pub fn new() -> Self {
        List{
            head_link: None,
            size: 0,
        }  
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn peek(&self) -> Option<Armor> {
        if self.size == 0 {
            return None
        }
        else {
            return self.head_link.as_ref().map(|a| a.try_read().unwrap().data)
        }
    }

    pub fn push(&mut self, component: Armor) {
        let new_node = Node {
            data: component,
            rest: self.head_link.clone(),
        }; 
        self.head_link = Some(Arc::new(RwLock::new(new_node)));
        self.size = self.size + 1;
    }

    pub fn pop(&mut self) -> Option<Armor> {
        if self.size == 0 {
            return None
        } else {
            let peek =  self.peek();
            self.head_link = self.head_link.as_ref().map(|a| a.try_read().unwrap().rest.clone()).unwrap();
            self.size = self.size - 1;
            return peek;
        }
    }
}

// Part 3

#[derive(Clone)]
pub struct Suit {
    pub armor: List,
    pub version: i32,
}

impl Suit {
    pub fn is_compatible(&self) -> bool {
        let h_ref = self.armor.head_link.as_ref();
        let mut vec = Vec::new();
        h_ref.map(|a| vec.push(a.try_read().unwrap().data.version));

        let len = vec.len();
        for i in 0..len {
            if vec[i] != self.version {
                return false
            }
        }
        return true;
    }

    pub fn repair(&mut self) {
        let mut armor_clone = self.armor.clone();
        let len = armor_clone.size();
        let mut i = 0;

        let mut vec = Vec::new();
        armor_clone.head_link.as_ref().map(|a| vec.push(a.try_read().unwrap().data.component));
        let len2 = vec.len();

        while (i < len) {     
            let option = &armor_clone.head_link;
            match option{
                Some(head) => {
                    let mut head = head.try_write().unwrap();
                    let poin = head.data.component;
                    match poin {
                        Component::LeftThrusters (true, percent) => {head.data.component = Component::LeftThrusters(false, 100);},
                        Component::LeftRepulsor (true, percent) => {head.data.component = Component::LeftRepulsor(false, 100);},
                        Component::RightRepulsor (true, percent) => {head.data.component = Component::RightRepulsor(false, 100);},  
                        Component::RightThrusters (true, percent) => {head.data.component = Component::RightThrusters(false, 100);},
                        Component::ChestPiece (true, percent) => {head.data.component = Component::ChestPiece(false, 100);},
                        _ => (),
                    }
                },
                None => (),
            }
            i+= 1;
            armor_clone.pop();
        }
    }
}
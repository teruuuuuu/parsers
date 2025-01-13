#[derive(Clone, PartialEq, Debug)]
pub struct ParseError {
    pub label: String,
    pub message: String,
    pub location: usize,
    pub children: Vec<ParseError>
} 

impl ParseError {

    pub fn new(label: String, message: String, location: usize, children: Vec<ParseError>) -> Self {
        ParseError {
            label,
            message,
            location,
            children
        }
    }

    pub fn last_error(&self) -> &ParseError {
        if self.children.len() == 0 {
            self
        } else {
            let c_last_opt = self.children.iter().map(|e| e.last_error()).max_by(|a, b| a.location.cmp(&b.location));
            if c_last_opt.is_some() && c_last_opt.unwrap().location > self.location  {
                c_last_opt.unwrap()
            } else {
                self
            }
        }
    }
}

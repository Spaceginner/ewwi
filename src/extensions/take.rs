pub trait Take<T> {
    fn take(&mut self) -> T;
}


impl<T> Take<T> for T
    where T: Default
{
    fn take(&mut self) -> T {
        std::mem::take(self)
    }
}

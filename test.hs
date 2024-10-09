-- succeeds
join_ :: Monad m => m (m a) -> m a
join_ = _


-- fails
fJoin :: (Monad m, Monad f) => f (m (m a)) -> f (m a)
fJoin = fmap _
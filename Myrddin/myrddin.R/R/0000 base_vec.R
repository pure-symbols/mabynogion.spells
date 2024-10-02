warp_if = function (cond) function (warpper) if (cond) warpper else base::identity
warpper_vec = function (.vec_ref) warp_if(base::length(.vec_ref) > 1)(base::Vectorize)

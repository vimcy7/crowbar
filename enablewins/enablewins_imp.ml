module type COUNTER = sig
  val t : int
end

module Make (Counter : COUNTER) = struct

  type result = Enable | Disable 
  
  let enable c = c + 1
  
  let disable c = c - c

  let is_enable c = (c > 0)

  let is_disable c = (c <= 0)

  let flag c = if (c > 0) then Enable else Disable

  let enablewins merge a b = 
    if (is_enable a || is_enable b) then (is_enable merge) else (is_disable merge)

  let disablewins merge a b = 
      if (is_disable a && is_disable b) then (is_disable merge) else (is_enable merge)

  let merge lca a b =
    let l = if (a <= lca) then 0 else (a - lca) in
    let r = if (b <= lca) then 0 else (b - lca) in
    let res = (l + r + lca) in
    if ((a <= 0) && (b <= 0)) then 0 else res
  end

   (*let merge lca a b =
      let l = (a - lca) in 
      let r = (b - lca) in 
      let res = (l + r + lca) in
      if (((l <= 0) && (r <= 0 )) || (res <= 0)) then 0 else res
     end*)




 



 

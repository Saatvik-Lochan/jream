{module, ring}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{start,2}]}.

{attributes, []}.

{labels, 25}.


{function, start, 2, 2}.
  {label,1}.
    {line,[{location,"ring.erl",4}]}.
    {func_info,{atom,ring},{atom,start},2}.
  {label,2}.
    {allocate,1,2}.
    {move,{x,1},{y,0}}.
    {bif,self,{f,0},[],{x,1}}.
    {line,[{location,"ring.erl",5}]}.
    {call,2,{f,7}}. % spawn_ring/2
    {'%',{var_info,{x,0},[{type,pid}]}}.
    {test_heap,3,1}.
    {put_tuple2,{x,1},{list,[{atom,pass},{y,0}]}}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"ring.erl",6}]}.
    send.
  {label,3}.
    {loop_rec,{f,5},{x,0}}.
    {test,is_eq_exact,{f,4},[{x,0},{atom,done}]}.
    remove_message.
    {move,{atom,ok},{x,0}}.
    {deallocate,1}.
    return.
  {label,4}.
    {loop_rec_end,{f,3}}.
  {label,5}.
    {wait,{f,3}}.


{function, spawn_ring, 2, 7}.
  {label,6}.
    {line,[{location,"ring.erl",14}]}.
    {func_info,{atom,ring},{atom,spawn_ring},2}.
  {label,7}.
    {'%',{var_info,{x,1},[{type,pid}]}}.
    {test,is_eq_exact,{f,8},[{x,0},{integer,1}]}.
    {test_heap,{alloc,[{words,1},{floats,0},{funs,1}]},2}.
    {make_fun3,{f,24},0,0,{x,0},{list,[{x,1}]}}.
    {line,[{location,"ring.erl",15}]}.
    {call_ext_only,1,{extfunc,erlang,spawn,1}}.
  {label,8}.
    {line,[{location,"ring.erl",17}]}.
    {gc_bif,'-',{f,0},2,[{x,0},{integer,1}],{x,0}}.
    {allocate,1,2}.
    {move,{x,1},{y,0}}.
    {call,2,{f,7}}. % spawn_ring/2
    {'%',{var_info,{x,0},[{type,pid}]}}.
    {test_heap,{alloc,[{words,2},{floats,0},{funs,1}]},1}.
    {make_fun3,{f,22},0,0,{x,1},{list,[{y,0},{x,0}]}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"ring.erl",18}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {test_heap,3,1}.
    {put_tuple2,{x,1},{list,[{atom,next},{x,0}]}}.
    {swap,{y,0},{x,0}}.
    {line,[{location,"ring.erl",19}]}.
    send.
    {move,{y,0},{x,0}}.
    {deallocate,1}.
    return.


{function, loop, 2, 10}.
  {label,9}.
    {line,[{location,"ring.erl",23}]}.
    {func_info,{atom,ring},{atom,loop},2}.
  {label,10}.
    {'%',{var_info,{x,0},[{type,pid}]}}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
  {label,11}.
    {loop_rec,{f,16},{x,0}}.
    {test,is_tuple,{f,15},[{x,0}]}.
    {test,test_arity,{f,15},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {select_val,{x,1},{f,15},{list,[{atom,next},{f,14},{atom,pass},{f,12}]}}.
  {label,12}.
    {test,is_eq_exact,{f,13},[{x,0},{integer,1}]}.
    remove_message.
    {move,{atom,done},{x,1}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"ring.erl",29}]}.
    send.
    {deallocate,2}.
    return.
  {label,13}.
    remove_message.
    {line,[{location,"ring.erl",32}]}.
    {gc_bif,'-',{f,0},1,[{x,0},{integer,1}],{x,0}}.
    {test_heap,3,1}.
    {put_tuple2,{x,1},{list,[{atom,pass},{x,0}]}}.
    {move,{y,0},{x,0}}.
    send.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,10},2}. % loop/2
  {label,14}.
    {move,{x,0},{x,1}}.
    remove_message.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,10},2}. % loop/2
  {label,15}.
    {loop_rec_end,{f,11}}.
  {label,16}.
    {wait,{f,11}}.


{function, module_info, 0, 18}.
  {label,17}.
    {line,[]}.
    {func_info,{atom,ring},{atom,module_info},0}.
  {label,18}.
    {move,{atom,ring},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 20}.
  {label,19}.
    {line,[]}.
    {func_info,{atom,ring},{atom,module_info},1}.
  {label,20}.
    {move,{x,0},{x,1}}.
    {move,{atom,ring},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-spawn_ring/2-fun-1-', 2, 22}.
  {label,21}.
    {line,[{location,"ring.erl",18}]}.
    {func_info,{atom,ring},{atom,'-spawn_ring/2-fun-1-'},2}.
  {label,22}.
    {'%',{var_info,{x,0},[{type,pid}]}}.
    {'%',{var_info,{x,1},[{type,pid}]}}.
    {call_only,2,{f,10}}. % loop/2


{function, '-spawn_ring/2-fun-0-', 1, 24}.
  {label,23}.
    {line,[{location,"ring.erl",15}]}.
    {func_info,{atom,ring},{atom,'-spawn_ring/2-fun-0-'},1}.
  {label,24}.
    {'%',{var_info,{x,0},[{type,pid}]}}.
    {move,{atom,undefined},{x,1}}.
    {call_only,2,{f,10}}. % loop/2

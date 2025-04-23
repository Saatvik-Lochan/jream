{module, ring}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{run,2}]}.

{attributes, []}.

{labels, 30}.


{function, run, 2, 2}.
  {label,1}.
    {line,[{location,"ring.erl",4}]}.
    {func_info,{atom,ring},{atom,run},2}.
  {label,2}.
    {allocate,4,2}.
    {init_yregs,{list,[{y,0}]}}.
    {move,{x,1},{y,2}}.
    {move,{x,0},{y,3}}.
    {bif,self,{f,0},[],{y,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"ring.erl",7}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,1}}.
    {init_yregs,{list,[{y,1}]}}.
    {call,2,{f,25}}. % '-run/2-lc$^0/1-0-'/2
    {'%',{var_info,{x,0},[{type,{t_list,pid,nil}}]}}.
    {move,{x,0},{y,1}}.
    {test,is_nonempty_list,{f,6},[{x,0}]}.
    {test_heap,2,0}.
    {get_list,{y,1},{y,0},{x,0}}.
    {put_list,{y,0},nil,{x,1}}.
    {line,[{location,"ring.erl",9}]}.
    {call_ext,2,{extfunc,erlang,'++',2}}.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"ring.erl",10}]}.
    {call_ext,2,{extfunc,lists,zip,2}}.
    {call,1,{f,22}}. % '-run/2-lc$^2/1-1-'/1
    {'%',{var_info,{x,0},[{type,nil}]}}.
    {line,[{location,"ring.erl",13}]}.
    {gc_bif,'*',{f,0},0,[{y,3},{y,2}],{x,1}}.
    {move,{y,0},{x,0}}.
    {init_yregs,{list,[{y,0},{y,2},{y,3}]}}.
    send.
  {label,3}.
    {loop_rec,{f,5},{x,0}}.
    {test,is_eq_exact,{f,4},[{x,0},{atom,done}]}.
    remove_message.
    {move,{y,1},{x,0}}.
    {call_last,1,{f,19},4}. % '-run/2-lc$^3/1-2-'/1
  {label,4}.
    {loop_rec_end,{f,3}}.
  {label,5}.
    {wait,{f,3}}.
  {label,6}.
    {line,[{location,"ring.erl",8}]}.
    {badmatch,nil}.


{function, proc, 1, 8}.
  {label,7}.
    {line,[{location,"ring.erl",22}]}.
    {func_info,{atom,ring},{atom,proc},1}.
  {label,8}.
    {'%',{var_info,{x,0},[{type,pid}]}}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
  {label,9}.
    {loop_rec,{f,13},{x,0}}.
    {test,is_tuple,{f,10},[{x,0}]}.
    {test,is_tagged_tuple,
          {f,12},
          [{tr,{x,0},{t_tuple,0,false,#{}}},2,{atom,next}]}.
    remove_message.
    {get_tuple_element,{x,0},1,{x,1}}.
    {move,{atom,next},{x,0}}.
    {line,[{location,"ring.erl",27}]}.
    {call_ext,2,{extfunc,erlang,put,2}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,8},1}. % proc/1
  {label,10}.
    {test,is_atom,{f,11},[{x,0}]}.
    {test,is_eq_exact,{f,12},[{tr,{x,0},{t_atom,any}},{atom,quit}]}.
    remove_message.
    {move,{atom,void},{x,0}}.
    {deallocate,1}.
    return.
  {label,11}.
    {test,is_eq_exact,{f,12},[{x,0},{integer,0}]}.
    remove_message.
    {move,{atom,done},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"ring.erl",30}]}.
    send.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,8},1}. % proc/1
  {label,12}.
    remove_message.
    {bif,get,{f,0},[{atom,next}],{x,1}}.
    {line,[{location,"ring.erl",33}]}.
    {gc_bif,'-',{f,0},2,[{x,0},{integer,1}],{x,0}}.
    {swap,{x,0},{x,1}}.
    send.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,8},1}. % proc/1
  {label,13}.
    {wait,{f,9}}.


{function, module_info, 0, 15}.
  {label,14}.
    {line,[]}.
    {func_info,{atom,ring},{atom,module_info},0}.
  {label,15}.
    {move,{atom,ring},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 17}.
  {label,16}.
    {line,[]}.
    {func_info,{atom,ring},{atom,module_info},1}.
  {label,17}.
    {move,{x,0},{x,1}}.
    {move,{atom,ring},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-run/2-lc$^3/1-2-', 1, 19}.
  {label,18}.
    {line,[{location,"ring.erl",20}]}.
    {func_info,{atom,ring},{atom,'-run/2-lc$^3/1-2-'},1}.
  {label,19}.
    {'%',{var_info,{x,0},[{type,{t_list,pid,nil}}]}}.
    {test,is_nonempty_list,{f,20},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {move,{atom,quit},{x,1}}.
    send.
    {swap,{y,0},{x,0}}.
    {call,1,{f,19}}. % '-run/2-lc$^3/1-2-'/1
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,20}.
    return.


{function, '-run/2-lc$^2/1-1-', 1, 22}.
  {label,21}.
    {line,[{location,"ring.erl",10}]}.
    {func_info,{atom,ring},{atom,'-run/2-lc$^2/1-1-'},1}.
  {label,22}.
    {'%',{var_info,{x,0},
                   [{type,{t_list,{t_tuple,2,true,#{1 => pid,2 => pid}},
                                  nil}}]}}.
    {test,is_nonempty_list,{f,23},[{x,0}]}.
    {allocate_heap,1,3,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {put_tuple2,{x,1},{list,[{atom,next},{x,1}]}}.
    {get_tuple_element,{x,0},0,{x,0}}.
    send.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,22},1}. % '-run/2-lc$^2/1-1-'/1
  {label,23}.
    return.


{function, '-run/2-lc$^0/1-0-', 2, 25}.
  {label,24}.
    {line,[{location,"ring.erl",7}]}.
    {func_info,{atom,ring},{atom,'-run/2-lc$^0/1-0-'},2}.
  {label,25}.
    {'%',{var_info,{x,1},[{type,pid}]}}.
    {test,is_nonempty_list,{f,26},[{x,0}]}.
    {allocate_heap,2,{alloc,[{words,1},{floats,0},{funs,1}]},2}.
    {move,{x,1},{y,1}}.
    {get_tl,{x,0},{y,0}}.
    {make_fun3,{f,29},0,0,{x,0},{list,[{x,1}]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_atom,[void]}}]}}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{y,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{x,0},{y,1}}.
    {trim,1,1}.
    {move,{x,2},{x,0}}.
    {call,2,{f,25}}. % '-run/2-lc$^0/1-0-'/2
    {'%',{var_info,{x,0},[{type,{t_list,pid,nil}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,26}.
    {test,is_nil,{f,27},[{x,0}]}.
    return.
  {label,27}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,bad_generator},{x,0}]}}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, '-run/2-fun-1-', 1, 29}.
  {label,28}.
    {line,[{location,"ring.erl",7}]}.
    {func_info,{atom,ring},{atom,'-run/2-fun-1-'},1}.
  {label,29}.
    {'%',{var_info,{x,0},[{type,pid}]}}.
    {call_only,1,{f,8}}. % proc/1

{module, merge}.  %% version = 0

{exports, [{main,0},
           {main_s,0},
           {main_split,0},
           {module_info,0},
           {module_info,1},
           {test,0}]}.

{attributes, []}.

{labels, 38}.


{function, main, 0, 2}.
  {label,1}.
    {line,[{location,"merge.erl",4}]}.
    {func_info,{atom,merge},{atom,main},0}.
  {label,2}.
    {allocate,0,0}.
    {move,{literal,"list.txt"},{x,0}}.
    {line,[{location,"merge.erl",5}]}.
    {call_ext,1,{extfunc,file,consult,1}}.
    {test,is_tagged_tuple,{f,3},[{x,0},2,{atom,ok}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {test,is_nonempty_list,{f,3},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,1}}.
    {test,is_nil,{f,3},[{x,1}]}.
    {move,{x,2},{x,0}}.
    {line,[{location,"merge.erl",6}]}.
    {call,1,{f,13}}. % merge_sort/1
    {call_ext_last,1,{extfunc,io,write,1},0}.
  {label,3}.
    {line,[{location,"merge.erl",5}]}.
    {badmatch,{x,0}}.


{function, main_s, 0, 5}.
  {label,4}.
    {line,[{location,"merge.erl",8}]}.
    {func_info,{atom,merge},{atom,main_s},0}.
  {label,5}.
    {allocate,0,0}.
    {move,{literal,"list.txt"},{x,0}}.
    {line,[{location,"merge.erl",9}]}.
    {call_ext,1,{extfunc,file,consult,1}}.
    {test,is_tagged_tuple,{f,6},[{x,0},2,{atom,ok}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {test,is_nonempty_list,{f,6},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,1}}.
    {test,is_nil,{f,6},[{x,1}]}.
    {move,{x,2},{x,0}}.
    {line,[{location,"merge.erl",10}]}.
    {call,1,{f,18}}. % sort/1
    {call_ext_last,1,{extfunc,io,write,1},0}.
  {label,6}.
    {line,[{location,"merge.erl",9}]}.
    {badmatch,{x,0}}.


{function, main_split, 0, 8}.
  {label,7}.
    {line,[{location,"merge.erl",12}]}.
    {func_info,{atom,merge},{atom,main_split},0}.
  {label,8}.
    {allocate,0,0}.
    {move,{literal,"list.txt"},{x,0}}.
    {line,[{location,"merge.erl",13}]}.
    {call_ext,1,{extfunc,file,consult,1}}.
    {test,is_tagged_tuple,{f,9},[{x,0},2,{atom,ok}]}.
    {get_tuple_element,{x,0},1,{x,1}}.
    {test,is_nonempty_list,{f,9},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,1}}.
    {test,is_nil,{f,9},[{x,1}]}.
    {move,{x,2},{x,0}}.
    {line,[{location,"merge.erl",14}]}.
    {call,1,{f,22}}. % split/1
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,2,true,
                                   #{1 => {t_list,any,nil},
                                     2 => {t_list,any,nil}}}}]}}.
    {call_ext_last,1,{extfunc,io,write,1},0}.
  {label,9}.
    {line,[{location,"merge.erl",13}]}.
    {badmatch,{x,0}}.


{function, test, 0, 11}.
  {label,10}.
    {line,[{location,"merge.erl",16}]}.
    {func_info,{atom,merge},{atom,test},0}.
  {label,11}.
    {allocate,0,0}.
    {move,{literal,"È"},{x,1}}.
    {move,{literal,"d"},{x,0}}.
    {line,[{location,"merge.erl",17}]}.
    {call,2,{f,28}}. % merge/2
    {'%',{var_info,{x,0},[{type,{t_cons,any,any}}]}}.
    {call_ext_last,1,{extfunc,io,write,1},0}.


{function, merge_sort, 1, 13}.
  {label,12}.
    {line,[{location,"merge.erl",20}]}.
    {func_info,{atom,merge},{atom,merge_sort},1}.
  {label,13}.
    {line,[{location,"merge.erl",21}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {test,is_ge,
          {f,14},
          [{integer,1},{tr,{x,1},{t_integer,{0,288230376151711743}}}]}.
    return.
  {label,14}.
    {allocate,1,1}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge.erl",24}]}.
    {call,1,{f,22}}. % split/1
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,2,true,
                                   #{1 => {t_list,any,nil},
                                     2 => {t_list,any,nil}}}}]}}.
    {test_heap,{alloc,[{words,2},{floats,0},{funs,1}]},1}.
    {bif,self,{f,0},[],{x,1}}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {make_fun3,{f,37},0,0,{x,1},{list,[{x,2},{x,1}]}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"merge.erl",26}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {get_tuple_element,{y,0},1,{x,0}}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge.erl",27}]}.
    {call,1,{f,13}}. % merge_sort/1
    {move,{x,0},{y,0}}.
  {label,15}.
    {loop_rec,{f,16},{x,0}}.
    remove_message.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,28},1}. % merge/2
  {label,16}.
    {wait,{f,15}}.


{function, sort, 1, 18}.
  {label,17}.
    {line,[{location,"merge.erl",34}]}.
    {func_info,{atom,merge},{atom,sort},1}.
  {label,18}.
    {test,is_nonempty_list,{f,19},[{x,0}]}.
    {get_tl,{x,0},{x,1}}.
    {test,is_nil,{f,20},[{x,1}]}.
    return.
  {label,19}.
    {test,is_nil,{f,20},[{x,0}]}.
    return.
  {label,20}.
    {allocate,1,1}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge.erl",37}]}.
    {call,1,{f,22}}. % split/1
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,2,true,
                                   #{1 => {t_list,any,nil},
                                     2 => {t_list,any,nil}}}}]}}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"merge.erl",38}]}.
    {call,1,{f,18}}. % sort/1
    {swap,{y,0},{x,0}}.
    {call,1,{f,18}}. % sort/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,28},1}. % merge/2


{function, split, 1, 22}.
  {label,21}.
    {line,[{location,"merge.erl",41}]}.
    {func_info,{atom,merge},{atom,split},1}.
  {label,22}.
    {line,[{location,"merge.erl",42}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {gc_bif,'div',
            {f,0},
            2,
            [{tr,{x,1},{t_integer,{0,288230376151711743}}},{integer,2}],
            {x,1}}.
    {swap,{x,0},{x,1}}.
    {call_only,2,{f,24}}. % split/2


{function, split, 2, 24}.
  {label,23}.
    {line,[{location,"merge.erl",44}]}.
    {func_info,{atom,merge},{atom,split},2}.
  {label,24}.
    {'%',{var_info,{x,0},[{type,{t_integer,{'-inf',144115188075855871}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_list,any,nil}}]}}.
    {test,is_nil,{f,25},[{tr,{x,1},{t_list,any,nil}}]}.
    {move,{literal,{[],[]}},{x,0}}.
    return.
  {label,25}.
    {test,is_eq_exact,
          {f,26},
          [{tr,{x,0},{t_integer,{'-inf',144115188075855871}}},{integer,0}]}.
    {test_heap,3,2}.
    {put_tuple2,{x,0},{list,[nil,{x,1}]}}.
    return.
  {label,26}.
    {get_list,{x,1},{x,2},{x,3}}.
    {test,is_ge,
          {f,23},
          [{tr,{x,0},{t_integer,{'-inf',144115188075855871}}},{integer,1}]}.
    {allocate,1,4}.
    {move,{x,2},{y,0}}.
    {line,[{location,"merge.erl",47}]}.
    {gc_bif,'-',
            {f,0},
            4,
            [{tr,{x,0},{t_integer,{1,144115188075855871}}},{integer,1}],
            {x,0}}.
    {move,{x,3},{x,1}}.
    {call,2,{f,24}}. % split/2
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,2,true,
                                   #{1 => {t_list,any,nil},
                                     2 => {t_list,any,nil}}}}]}}.
    {test_heap,5,1}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_tuple2,{x,0},{list,[{x,1},{x,0}]}}.
    {deallocate,1}.
    return.


{function, merge, 2, 28}.
  {label,27}.
    {line,[{location,"merge.erl",51}]}.
    {func_info,{atom,merge},{atom,merge},2}.
  {label,28}.
    {test,is_nil,{f,29},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,29}.
    {test,is_nil,{f,30},[{x,1}]}.
    return.
  {label,30}.
    {test,is_nonempty_list,{f,27},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_nonempty_list,{f,27},[{x,1}]}.
    {get_list,{x,1},{x,4},{x,5}}.
    {test,is_ge,{f,31},[{x,4},{x,2}]}.
    {allocate,1,4}.
    {move,{x,2},{y,0}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"merge.erl",55}]}.
    {call,2,{f,28}}. % merge/2
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,31}.
    {allocate,1,6}.
    {move,{x,4},{y,0}}.
    {move,{x,5},{x,1}}.
    {line,[{location,"merge.erl",56}]}.
    {call,2,{f,28}}. % merge/2
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,any},
                                   {t_cons,any,any},
                                   {t_number,any},
                                   {t_tuple,0,false,#{}},
                                   other}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, module_info, 0, 33}.
  {label,32}.
    {line,[]}.
    {func_info,{atom,merge},{atom,module_info},0}.
  {label,33}.
    {move,{atom,merge},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 35}.
  {label,34}.
    {line,[]}.
    {func_info,{atom,merge},{atom,module_info},1}.
  {label,35}.
    {move,{x,0},{x,1}}.
    {move,{atom,merge},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-merge_sort/1-fun-0-', 2, 37}.
  {label,36}.
    {line,[{location,"merge.erl",26}]}.
    {func_info,{atom,merge},{atom,'-merge_sort/1-fun-0-'},2}.
  {label,37}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {'%',{var_info,{x,1},[{type,pid}]}}.
    {allocate,1,2}.
    {move,{x,1},{y,0}}.
    {call,1,{f,13}}. % merge_sort/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    send.
    {deallocate,1}.
    return.

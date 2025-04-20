{module, gray}.  %% version = 0

{exports, [{main,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 12}.


{function, main, 0, 2}.
  {label,1}.
    {line,[{location,"gray.erl",4}]}.
    {func_info,{atom,gray},{atom,main},0}.
  {label,2}.
    {allocate,0,0}.
    {move,{integer,0},{x,1}}.
    {move,{integer,10},{x,2}}.
    {move,{integer,0},{x,0}}.
    {line,[{location,"gray.erl",5}]}.
    {call,3,{f,6}}. % gray_burn/3
    {'%',{var_info,{x,0},[{type,{t_integer,{0,15}}}]}}.
    {call_ext_last,1,{extfunc,io,write,1},0}.


{function, gray, 1, 4}.
  {label,3}.
    {line,[{location,"gray.erl",7}]}.
    {func_info,{atom,gray},{atom,gray},1}.
  {label,4}.
    {'%',{var_info,{x,0},[{type,{t_integer,{0,10}}}]}}.
    {gc_bif,'bsr',{f,0},1,[{tr,{x,0},{t_integer,{0,10}}},{integer,1}],{x,1}}.
    {gc_bif,'bxor',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,{0,10}}},{tr,{x,1},{t_integer,{0,5}}}],
            {x,0}}.
    return.


{function, gray_burn, 3, 6}.
  {label,5}.
    {line,[{location,"gray.erl",9}]}.
    {func_info,{atom,gray},{atom,gray_burn},3}.
  {label,6}.
    {'%',{var_info,{x,0},[{type,{t_integer,{0,'+inf'}}}]}}.
    {'%',{var_info,{x,1},[{type,{t_integer,{0,15}}}]}}.
    {'%',{var_info,{x,2},[{type,{t_integer,{10,10}}}]}}.
    {test,is_ge,{f,7},[{tr,{x,0},{t_integer,{0,'+inf'}}},{integer,11}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,7}.
    {allocate,2,2}.
    {init_yregs,{list,[{y,0}]}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"gray.erl",11}]}.
    {gc_bif,'+',{f,0},1,[{tr,{x,0},{t_integer,{0,10}}},{integer,1}],{y,0}}.
    {call,1,{f,4}}. % gray/1
    {'%',{var_info,{x,0},[{type,{t_integer,{0,15}}}]}}.
    {gc_bif,'bxor',
            {f,0},
            1,
            [{tr,{y,1},{t_integer,{0,15}}},{tr,{x,0},{t_integer,{0,15}}}],
            {x,1}}.
    {move,{integer,10},{x,2}}.
    {move,{y,0},{x,0}}.
    {call_last,3,{f,6},2}. % gray_burn/3


{function, module_info, 0, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,gray},{atom,module_info},0}.
  {label,9}.
    {move,{atom,gray},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 11}.
  {label,10}.
    {line,[]}.
    {func_info,{atom,gray},{atom,module_info},1}.
  {label,11}.
    {move,{x,0},{x,1}}.
    {move,{atom,gray},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.

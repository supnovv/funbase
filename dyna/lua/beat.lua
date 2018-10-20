--[[
不重用的另一种方法：服务保存两个域，一个是 index，一个 count，count表示从开机时的服务计数

Lua 的 coroutine 可以帮助我们把一个在 C 层面分离的 callback 调用串成逻辑上连续的线
索。当 Lua 编写的服务接收到一个外部请求时，对应底层 callbcak 函数调用，然后转发到
lua 虚拟机中，skynet 的 lua 层会为请求创建一个独立的 coroutine。一旦在处理这个请求
的 coroutine 中发生远程调用，即发出一个消息包，coroutine 会挂起。在 C 层面，这次
callback 正常返回，但在 lua 中，则是记录下这个发出的消息包的 session，记录 session
和挂起的 coroutine 在一张对应的表中。之后，一旦收到回应包里有相同的 session，对应
的 coroutine 则被唤醒 resume。

Lua服务依附在C服务 snlua 上运行，Lua服务每收到一个消息，都将分配一个 coroutine 来
执行，在 coroutine 可以执行任何代码，根据情况 coroutine 会被挂起或执行完毕。挂起
的协程被维护起来等待唤醒，而执行完毕的协程再被释放重用（协程号不重用）。

协程被挂起的情况：
1. Lua 服务发送了一个需要回复的消息，在发送后协程被 C 服务挂起，直到收到消息的回复（只会等这个消息，如果是其他消息到来，将丢弃）
2. Lua 服务不发送消息，但主动等待一个消息的到来（只会等待这个特定消息，如果其他消息到来，将丢弃）
3. Lua 服务自己挂起自己，此状态下等待的协程不会接收消息，只能等其他协程唤醒自己 (TODO)

如果 Lua 服务发送一个不需要回复的消息，它会继续执行，不会被挂起
如果在协程一个次执行中，即没有发送需要回复的消息，Lua 服务自己也没有将自己挂起，测
协程将执行完毕，执行完毕的协程将被释放

协程只能挂起自己，不能挂起其他协程，挂起的协程只能被其他协程（包括主协程）唤醒
协程有：suspended, running, normal, dead
新创建的协程或挂起的协程处于 suspended 状态，resume 新创建的或挂起的协程处于 running
状态，正常执行完毕的协程处于 dead 状态，dead 状态的协程不能再次被 resume　(??? 真的假的，是否只有 LUA 层是这样，C 层实际上是可以重复利用的 ???）
一个正在运行的协程如果 resume 其他协程，则这个协程处于 normal 状态，该协程在被唤醒的
协程运行完毕后会继续执行

协程被唤醒的情况：
1. LUA 服务对应协程收到消息
   * 如果该协程在等待该消息则接收，否则将消息丢弃继续等待
   * 如果该协程没有等待消息，消息被丢弃
2. 如果被其他协程唤醒直接唤醒 (TODO)

一个套接字只由一个服务负责处理，而一个服务可以有多个 coroutine，
一个 couroutine 可以线性的处理一件事情，每个服务都拥有一个独立的Lua虚拟机。
对于 HTTP server 服务，每收到一个连接都分配一个新的服务。
每发送一条消息需要回复的消息都新分配一个协程来处理，这个协程会在发送消息后挂起，等待消息的回应到来。
服务每收到一个消息，如果这个消息没有被任何协程等待，会被服务注册的消息函数来处理，这个新的处理也要在新的协程中处理，因为处理过程中可能被挂起。

注意如果是一个服务内部的协程相互将发送消息，直接保存在服务的消息列表中即可，
因为这个通信在一个线程内，直接操作即可

local https_svid = l_create_listen_service("127.0.0.1", 80)

Http Server 涉及到的服务：
1. 创建 Http Server 的服务 - 服务1 (服务的创建都由 launcher 服务来处理，需要创建的服务会配置在配置文件中）
2. 监听端口并接收连接的服务 - 服务2
3. 每个成功建立的连接都分配一个服务负责连接管理和数据读写

lua_State* lua_newstate(lua_Alloc f, void* ud);
lua_State* lua_newthread(lua_State* L);
---
The lua_newstate create a new thread (or a main coroutine) running in a new,
independent state. Returns NULL if it cannot create the thread. THe arguments
f is the allocator function; Lua does all memory allocation for this state
through this function. The second argument, ud, is an opaque pointer that Lua
passes to the allocator in every call.
The lua_newthread a new thread (or a new coroutine related to the main
coroutine), pushes it on the stack, and returns a pointer represents the new
coroutine. The returned coroutine shares with the main coroutine its global
environment, but has an independent execution stack. There is no explicit
function to close or to destroy a coroutine. Coroutines are subject to garbage
collection, like any Lua object.
---
int lua_resume(lua_State* co, lua_State* from, int nargs);
int lua_yield(lua_State* L, int nresults);
int lua_yieldk(lua_State* L, int nresults, lua_KContext ctx, lua_KFunction k);
---
The lua_resume function start or resume a coroutine. The parameter from
represents the coroutine that is resuming co. If there is no such coroutine,
this parameter can be NULL.
To start a coroutine, you push onto the stack the main function plus any
arguments; then you call lua_resume, with nargs being the number of arguments.
This call returns when the coroutine suspends or finishes its execution. When
it returns, the stack contains all values passed to lua_yield, or all values
returned by the main function. lua_resume return LUA_YIELD if the coroutine
yields, LUA_OK if the coroutine finishes its execution without errors, or an
error code in case of errors. In case of errors, the stack is not unwound, so
you can use the debug API over it. The error object is on the top of the stack.
To resume a coroutine, you remove any results from the last lua_yield, put on
its stack only the values to be passed as results from yield, and then call
lua_resume.
The lua_yield function is equivalent to lua_yieldk, but it has no continuation.
Therefore, when the thread resumes, it continues the function that called the
function calling lua_yield.
@nresults, number of results on the stack. These results will be returned to
the caller who called the lua_resume to resume this coroutine.
---

协程创建->执行->挂起->唤醒->执行完毕的流程
----------------------------------------

协程执行 LUA 函数的情况，以主协程创建新协程为例
a1. 主协程调用 lua_newthread 创建新协程，新创建协程处于 suspended 状态
a2. 主协程将 LUA 函数和参数推入新协程栈，并调用 lua_resume 运行新协程
a3. lua_resume 保存执行环境设置恢复点，然后运行 LUA 函数
a4. 运行的新协程进入 running 状态，而主协程进入 normal 状态
a5. LUA 函数中的代码调用 yield 挂起协程，实际调到 lua_yield 执行协程挂起
a6. 挂起协程时，首先将 yield 的参数作为返回结果推入协程栈，然后 lua_yield 被调用
a7. lua_yield 执行 longjmp 回到 lua_resume 设置的恢复点，lua_resume 返回
a8. lua_resume 返回时拿到 yield 推入的返回结果并回到主协程
a9. 主协程进入 running 状态，挂起的协程进入 suspended 状态
b0. 主协程将新的参数推入挂起的协程栈，调用 lua_resume 唤醒挂起的协程
b1. lua_resume 保存执行环境设置恢复点，然后继续执行 LUA 函数没完的部分
b2. 当 LUA 函数继续执行时，相当于 yield 函数返回了，并拿到新推入的参数作为其返回结果
b3. 然后 LUA 函数继续执行 yield 后面未完的代码
b4. 主线程进入 normal 状态，挂起的协程恢复到 running 状态
b5. LUA 函数如果继续挂起协程，流程回到 a5，否则 LUA 函数执行完毕
b6. 执行完毕的 LUA 函数会将函数的返回结果推入协程栈，然后 lua_resume 自然返回
b7. lua_resume 返回时拿到 LUA 函数的返回结果并回到主协程
b8. 主线程进入 running 状态，新协程执行完毕进入 dead 状态

#define LUAI_TRY(co,lj,a) if (setjmp((lj)->b) == 0) { a }
#define LUAI_THROW(co,lj) longjmp((lj)->b, 1)
#define luai_jmpbuf jmp_buf

struct lua_longjmp {
  struct lua_longjmp* previous;
  luai_jmpbuf b;
  volatile int status; /* error code */
};

=> lua_resume(co, from, nargs) start the coroutine
   luaD_rawrunprotected(co, resume, &nargs)
   LUAI_TRY(co, lj, resume(co, &nargs); )
   if (setjmp(lj->b) == 0) {
     resume(co, &nargs): /* starting the coroutine */
       StkId firstArg = L->top - nargs; /* first argument */
       StkId func = firstArg - 1; /* the lua function stack index */
       luaD_precall(co, func, LUA_MULTRET): /* prepare the lua function call */
       luaV_execute(co); /* execute the lua function */
   }

=> lua_resume(co, from, nargs) resume the coroutine
   luaD_rawrunprotected(co, resume, &nargs)
   LUAI_TRY(co, lj, resume(co, &nargs); )
   if (setjmp(lj->b) == 0) {
     resume(co, &nargs): /* resuming the coroutine */
       luaD_poscall(co, ci, firstArg, nargs)
       luaV_finishOp(co)
       luaV_execute(co)
   }

=> lua_yield(co, nresults)
   lua_yieldk(co, nresults, 0, NULL)
   luaD_throw(co, LUA_YIELD)
   co->errorJmp->status = LUA_YIELD
   LUAI_THROW(co, co->errorJmp);
   longjmp(jp->b, 1);

lua_resume 核心逻辑如上，一开始调用 setjmp 保存执行环境，并在 resume 中运行 LUA
函数（因为 setjmp 第一次返回会返回 0），如果 LUA 函数没有执行挂起，resume 执行完毕，
lua_resume自然返回。如果 LUA 函数执行了挂起，代码执行流程将返回到 setjmp，此时的
setjmp 是第二次返回，返回一个非 0 值，然后 lua_resume 返回。

coroutine.create(f)
coroutine.resume(co)
coroutine.resume(co, val1, ...)
coroutine.yield()
coroutine.yield(val1, ...)
coroutine.isyieldable() /* a running coroutine is yieldable if it is not the main thread and it is not inside a non-yieldable C function */
coroutine.running()
coroutine.status(co)

typedef int (*lua_CFunction)(lua_State* L);
---
In order to communicate properly with Lua, a C function must use the following
protocol, which defines the way parameters and returns are passed:
1. a C function receives its arguments from Lua in its stack in direct order
(the first argument is pushed first)
2. so when the function starts, lua_gettop(L) returns the number of arguments
received by the function. the first argument (if any) is at index 1 and its
last argument is at index lua_gettop(L)
3. to return values to Lua, a C function just pushes them onto the stack, in
direct order (the first result is pushed first), and returns the number of
results.
4. any other value in the stack below the results will be properly discarded
by the Lua.


LUA 虚拟机调用 LUA 函数 lua_func(a, b, c) 的过程（lua_func 在底层通过 c_func 实现）
1. LUA 虚拟机将 lua_func 和 3 个 LUA 值 a, b, c 作为参数压入栈中
2. 调用 lua_call 或者 lua_pcall 执行函数，由于 lua_func 对应的是一个 C 函数
3. C 函数将被调用，C 函数通过 lua_gettop(L) 可以得知参数的个数
4. C 函数执行其想要的操作，然后将结果压入栈中，并返回结果个数
5. LUA 虚拟机获知结果个数，将所有结果作为 lua_func 返回值返回，并清理栈中不再用的值

LUA 中的 C 函数必须借助 countinue function 实现 C 函数继续执行，
不能像 LUA 函数一样，唤醒的协程会继续未完的 C 函数代码。如果实现
真正的 C 协程，需要借助 WINDOWS 的纤程或操作系统类似的机制。

]]


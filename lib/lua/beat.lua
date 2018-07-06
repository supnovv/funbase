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

注意如果是一个服务内部的协程相互将发送消息，直接保存在服务的消息列表中即可，
因为这个通信在一个线程内，直接操作即可


int lua_yield(lua_State* L, int nresults);
int lua_yieldk(lua_State* L, int nresults, lua_KContext ctx, lua_KFunction k);

The lua_yield function is equivalent to lua_yieldk, but it has no continuation.
Therefore, when the thread resumes, it continues the function that called the
function calling lua_yield.

@nresults, number of results on the stack. These results will be returned to
the caller who called the lua_resume to resume this coroutine.

The coroutine suspend and resome detail flows:
1.
2.

]]

local co = require "coroutine"



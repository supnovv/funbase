#ifndef LNLYLIB_OSI_WINDEFS_H
#define LNLYLIB_OSI_WINDEFS_H
#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#define _CRT_SECURE_NO_WARNINGS
/** Windows Pre-defined Macros **
WINVER: windows version - can used to enable features on specific versions
such as 0x0501 for Windows XP and 0x0600 for Windows Vista.
_WIN16/_WIN32/_WIN64: 16/32/64-bit windows platform.
_WIN32 is also defined on 64-bit platform for backward compatibility.
_M_IX86/_M_X64/_M_IA64: for x86/x64/(intel itanium) architecture.
*** Widows Docs & SDK & Tools **
https://docs.microsoft.com/en-us/windows/desktop/
https://developer.microsoft.com/en-us/windows
https://www.microsoft.com/en-us/download/default.aspx
**********************************************************************/
#include <windows.h>

/** all defines here must based on the os own definitions **/

#endif /* LNLYLIB_OSI_WINDEFS_H */


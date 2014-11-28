/*
 * Wrapper implementation for pipes
 *
 */
#include <AvailabilityMacros.h>
#include <sys/types.h>
#include <CoreFoundation/CoreFoundation.h>

typedef CFSocketNativeHandle PipeNativeHandle;
typedef CFSocketCallBack PipeCallBack;
typedef CFSocketCallBackType PipeCallBackType;
typedef CFSocketContext PipeContext;
typedef CFSocketRef PipeRef;

#define kPipeNoCallBack kCGSocketNoCallBack
#define kPipeReadCallBack kCFSocketReadCallBack
#define kPipeDataCallBack kCFSocketDataCallBack
#define kPipeWriteCallBack kCFSocketWriteCallBack

extern PipeRef PipeCreateWithNative(CFAllocatorRef allocator, 
				    PipeNativeHandle pipe, 
				    CFOptionFlags callBackTypes, 
				    PipeCallBack callback, const PipeContext *context);

extern CFRunLoopSourceRef PipeCreateRunLoopSource(CFAllocatorRef allocator, 
						  PipeRef pipe, CFIndex order);

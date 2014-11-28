#include <AvailabilityMacros.h>
#include <sys/types.h>
#include <CoreFoundation/CoreFoundation.h>

#include "Pipe.h"

PipeRef PipeCreateWithNative(CFAllocatorRef allocator, 
			     PipeNativeHandle pipe, 
			     CFOptionFlags callBackTypes, 
			     PipeCallBack callback, const PipeContext *context)
{
    return CFSocketCreateWithNative(allocator, pipe, 
				    callBackTypes,
				    callback,
				    context);
}

CFRunLoopSourceRef PipeCreateRunLoopSource(CFAllocatorRef allocator, 
					   PipeRef pipe, CFIndex order)
{
    return CFSocketCreateRunLoopSource(allocator, pipe, order);
}


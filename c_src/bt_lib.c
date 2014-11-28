#include <AvailabilityMacros.h>
#include <sys/types.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOBluetooth/IOBluetoothUserLib.h>
#include <stdio.h>
#include <stdlib.h>

const char* format_str(CFStringRef cfstr, char* buf, int len)
{
    if (cfstr == NULL)
	return NULL;
    else {
	const char *ptr = CFStringGetCStringPtr(cfstr, kCFStringEncodingUTF8);
	if (ptr == NULL) {
	    if (CFStringGetCString(cfstr,buf,len,kCFStringEncodingUTF8))
		return buf;
	}
	return ptr;
    }
}

const char* format_date_tz(CFDateRef theDate, 
			   CFTimeZoneRef tz, char* buf, int len)
{
    CFAbsoluteTime at = CFDateGetAbsoluteTime(theDate);
    CFGregorianDate gdate = CFAbsoluteTimeGetGregorianDate(at, tz);

    snprintf(buf, len, "%04d-%02d-%02dT%02d:%02d:%02d",
	     gdate.year, gdate.month, gdate.day,
	     gdate.hour, gdate.minute, (int)gdate.second);
    return buf;
}

const char* format_date(CFDateRef theDate, char* buf, int len)
{
    format_date_tz(theDate, CFTimeZoneCopyDefault(), buf, len);
}

IOReturn string_to_addr(char* cstr_addr, BluetoothDeviceAddress* addr)
{
    CFStringRef str_addr = CFStringCreateWithCString(kCFAllocatorNull,
						     cstr_addr, 
						     kCFStringEncodingASCII);
    return IOBluetoothCFStringToDeviceAddress(str_addr, addr);
}

/* given bluetooh address find device in devices array */
IOBluetoothDeviceRef find_device_by_addr(CFArrayRef devices, 
					 BluetoothDeviceAddress* addr)
{
    CFIndex i = 0;
    CFIndex n = CFArrayGetCount(devices);

    while(i < n) {
	IOBluetoothDeviceRef device = (IOBluetoothDeviceRef)
	    CFArrayGetValueAtIndex(devices, i);
	if (device != NULL) {
	    const BluetoothDeviceAddress* bt_addr = 
		IOBluetoothDeviceGetAddress(device);
	    if (bt_addr != NULL) {
		if (memcmp(addr, bt_addr, sizeof(BluetoothDeviceAddress))==0)
		    return device;
	    }
	}
	i++;
    }
    return NULL;
}

/* given device name, find device in devices array */
IOBluetoothDeviceRef find_device_by_name(CFArrayRef devices, char* name)
{
    CFIndex i = 0;
    CFIndex n = CFArrayGetCount(devices);

    while(i < n) {
	IOBluetoothDeviceRef device = (IOBluetoothDeviceRef)
	    CFArrayGetValueAtIndex(devices, i);
	if (device != NULL) {
	    CFStringRef cfname = IOBluetoothDeviceGetName(device);
	    char name_buf[256];
	    const char* ptr;

	    if (name != NULL) {
		if ((ptr = format_str(cfname, name_buf,sizeof(name_buf))) != NULL)
		    if (strcmp(ptr, name) == 0)
			return device;
	    }
	}
	i++;
    }
    return NULL;
}

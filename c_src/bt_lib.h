
#include <sys/types.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOBluetooth/IOBluetoothUserLib.h>

extern const char* format_str(CFStringRef cfstr, char* buf, int len);
extern const char* format_date(CFDateRef theDate, char* buf, int len);
extern const char* format_date_tz(CFDateRef theDate, 
				  CFTimeZoneRef tz, char* buf, int len);


extern IOBluetoothDeviceRef find_device_by_name(CFArrayRef devices, 
						char* name);
extern IOBluetoothDeviceRef find_device_by_addr(CFArrayRef devices, 
						BluetoothDeviceAddress* addr);

extern IOReturn string_to_addr(char* cstr_addr, BluetoothDeviceAddress* addr);



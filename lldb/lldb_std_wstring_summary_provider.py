import ~/settings/lldb/lldb_wchar_t_summary_provider.py

def std_wstring_summary(value, unused):
    try:
        d = value.GetChildMemberWithName('__r_')
            .GetChildAtIndex(0)
            .GetChildMemberWithName('__value_')
            .GetChildMemberWithName('__l')
            .GetChildMemberWithName('__data_')
        return lldb_wchar_t_summary_provider.utf16(d)
    except:
        print '?????????????????????????'
        return value

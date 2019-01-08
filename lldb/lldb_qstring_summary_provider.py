# https://stackoverflow.com/questions/25370142/can-lldb-data-formatters-call-methods

def make_string_from_pointer_with_offset(F,OFFS,L):
    strval = 'u"'
    try:
        data_array = F.GetPointeeData(0, L).uint16
        for X in range(OFFS, L):
            V = data_array[X]
            if V == 0:
                break
            strval += unichr(V)
    except:
        pass
    strval = strval + '"'
    return strval.encode('utf-8')


#qt5
def qstring_summary(value, unused):
    try:
        d = value.GetChildMemberWithName('d')
        #have to divide by 2 (size of unsigned short = 2)
        offset = d.GetChildMemberWithName('offset').GetValueAsUnsigned() / 2
        size = get_max_size(value)
        return make_string_from_pointer_with_offset(d, offset, size)
    except:
        print '?????????????????????????'
        return value

def get_max_size(value):
    _max_size_ = None
    try:
        debugger = value.GetTarget().GetDebugger()
        _max_size_ = int(lldb.SBDebugger.GetInternalVariableValue('target.max-string-summary-length', debugger.GetInstanceName()).GetStringAtIndex(0))
    except:
        _max_size_ = 512
    return _max_size_

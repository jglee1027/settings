import lldb

def utf16(value, dict):
    e = lldb.SBError()
    out = u'"'
    if value.GetValue() != 0:
        i = 0
        ch = -1
        while ch != 0:
            data = value.GetPointeeData(i, 1)
            size = data.GetByteSize()
            j = 0
            while j < size:
                ch = data.GetUnsignedInt16(e, j)
                if e.fail:
                    return '<error>'
                j = j + 2
                if ch != 0:
                    out = out + unichr(ch)
            i = i + 1
    out = out + u'"'
    return out.encode('utf-8')

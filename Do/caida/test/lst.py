import re as regex

def get_block(end, f):
    ret = ""
    pre_pos = -1
    pos = f.tell()
    line = f.readline()
    
    while line:
        if len(regex.findall(end, line)) != 0:
           break
        
        ret = ret + line
        pre_pos = pos
        pos = f.tell()
        line = f.readline()
    
    f.seek(pre_pos)
    print(f.readline())
    f.seek(pre_pos)

    return ret

print(get_block("line 3", open("test_file")))

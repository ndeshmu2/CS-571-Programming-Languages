import json


def checkcharacter(ch):
    c = ch.isalpha()
    try:
        if c == True:
            return True
    except:
        return False

def checkinteger(check):
    try:
        if check == '0':
            return True
        if int(check):
            return True       
    except:
        return False

def opt(arr):    
    mark = 0; begin = 0; last = 0; s = 1; var = ""
    while s < len(arr): 
        if checkinteger(arr[s]) :
            var = var + arr[s]
            j = s+1
            if arr[j] == ']':
                begin = int(var)
                last = int(var)
                mark = j
                return begin,last,mark
            else:
                if arr[j] in [" ","."]:
                    print(arr[j])  
                    if var != "":
                        begin = int(var)
                        var = ""
                    j = j + 1
                    while arr[j] in [" ","."]:
                        j = j + 1
                    if arr[j] not in [" ","."]:
                        while arr[j] != "]":
                            
                            if checkinteger(arr[j]):
                                var = var + arr[j]
                            j = j + 1
                        if var != "":
                            last = int(var)
                            mark = j
                            return begin,last,mark
        else:
            pass
        s += 1    
    return begin,last,mark

def parse_nest(arr):
    parsed_num = []; mark = 0 ;var = "";c = 1
    while c < len(arr):
        while arr[c] != "}":
            if checkinteger(arr[c]):
                var = var + arr[c]
            elif arr[c] == "," :
                if var!= "":
                    parsed_num.append(int(var))
                    var = ""
            elif arr[c] == '[':            
                begin,last,mark = opt(arr[c:])
                c += mark
                c += 1
                if arr[c] == "=" and arr[c+1] !="{":

                    c += 1
                    while arr[c] != ',' and arr[c] != '}':
                        if checkinteger(arr[c]):
                            var = var + arr[c]

                            c += 1
                        else:
                            print(arr[c])

                            c += 1
                    if var != "":
                        if begin == last :
                            while len(parsed_num)<begin:
                                parsed_num.append(0)
                            parsed_num.append(int(var))
                            var = ""

                        else:
                            while len(parsed_num) < begin:
                                parsed_num.append(0)
                            while len(parsed_num) <= last:
                                parsed_num.append(int(var))
                                var = ""
                        if arr[c] == "}":
                            mark = c
                            return parsed_num,mark
                else:
                    pass
            elif arr[c] == '{':
                indexed = []
                indexed ,mark = parse_nest(arr[c:])
                parsed_num.append(indexed)

                c += mark

            c += 1

        if arr[c] == '}':
            if var != "":
                parsed_num.append(int(var))
                var = ""
                mark = c
                return parsed_num , mark
            else:
                mark = c
                return parsed_num,mark     
        c += 1  
    return parsed_num,mark 

def parse(arr):    
    j = 0; to_print = []; parsed_num = []; var = ""; i = 0; f = len(arr); 
    test1 = []; test2 = []; test3=[]
    if arr[0]=='{' and arr[-1] == '}':
        j = 1
        while j < len(arr):
            if arr[j] == '{' :
                if arr[j-1] != '=':
                    parsed_num = []
                    parsed_num,mark = parse_nest(arr[j:])
                    to_print.append(parsed_num)
                    j += mark

                    for f in range(3,8):
                        test2.append(f)

            elif checkinteger(arr[j]):
                var = var + arr[j]
            elif checkcharacter(arr[j]):
                print("garbage element in the array")
                exit()
            elif arr[j] == "," or arr[j] == '}' or arr[j] == " ":
                try :
                    if var != "":
                        to_print.append(int(var))
                        var = ""
                except:
                    pass
            elif arr[j] == '[':      
                var = ""     
                begin,last,mark = opt(arr[j:])
                j += mark
                j += 1
                while arr[j] in [" ","="]:
                    j += 1

                if arr[j] != "{":
                    while arr[j] not in [",","}"," "] :

                        if checkinteger(arr[j]):
                            var = var + arr[j]
                            j = j + 1

                        else:
                            break
                    if var != "":
                        if len(to_print)-1 == begin:
                            to_print[begin] = int(var)
                            var = ""

                            for h in range(1,9):
                                test1.append(h)
                        else:
                            while len(to_print) < begin:
                                to_print.append(0)
                            while len(to_print) <= last:
                                to_print.append(int(var))
                            var = " "

                            for p in range(1,6):
                                test3.append(p)

                elif arr[j] == "{":
                    print("nested array")
                    parsed_num = []
                    parsed_num,mark = parse_nest(arr[i:])
                    j += mark
                    while len(to_print) < begin:
                        to_print.append(0)
                    while len(to_print)<=last:
                        to_print.append(parsed_num)
                elif (arr[j].isalpha()):
                    print("Incorrect input")
            else:
                pass
            j += 1     
    elif checkinteger(arr):
        i = 1; to_print.append(int(arr))
        print("\n Given array ", arr )      
        print(" Output in JSON: ",to_print[0])
    elif f==len(arr):
        if(f!='}'):
            print("Unbalanced input")
            exit()  
    if i == 0:         
        print("\n\n Given array : ", arr )      
        print(" Output in JSON : ",json.dumps(to_print,indent=2))
inp = input()
parse(inp)

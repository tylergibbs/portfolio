import re as regex
import codecs
import itertools
NEW_WARC_STATMENT = "WARC/1.0" 
WARC_TYPE_REGEX = "^WARC-Type: (.+)"
#default parse_dic, parses file so each warc object all
#is stored

###################################################################################################################
#intructions
#these fucntions are placed in the "instr" dictionary
#they modify the return of re.findall before it is placed
#in the parced dictionary

#gets a block of text exculding the end line
#used when an instruction needs to get text over an
#arbitrary amound of lines terminated by the
#mathching of a regex
def get_block(end, f):
    ret = ""
    pre_pos = f.tell() # very hacky solution re itterates through
                       #block after becouse f.seek/f.tell was
                       # producing 1008 1008 1080 1080 1080 1080 ...
    line = f.readline()

    while line:
        #will continue untill the end of the file
        #a new warc statment or end is matched
        if len(regex.findall(end, line)) != 0 or \
           len(regex.findall(NEW_WARC_STATMENT, line)) != 0:
           break

        #adds the line to the return value
        ret = ret + line
        #and goes onto the next line
        line = f.readline()

    #resets the file pointer to the line after get_block
    #was triggured, none of the texted matched should
    #mathch the next regex else it will store it twice
    f.seek(pre_pos)
    return ret
        
           
def get_html_header(line, f):
    #gets block untill start of real html
    return line[0] + get_block("^<!DOCTYPE .+>", f)

def get_html(line, f):
    #gets html untill begining of next warc statment
    return line[0] + get_block("WARC_TYPE_REGEX", f)

#retreves only the www.example.com part of the webpage
#ignores variables, paths, ectera
def get_website_from_url(URL, f):
    return regex.findall("^.*//(.+?)([?/#].*)?$", URL[0])[0][0]

#default instruction, it retreves the first matched
#section and returns it
def defualt_intruction(line, f):
    return line[0]
###################################################################################################################
#these dictionaries instruct the parcer on what to stor from the warc file
#parse_warc_all saves the entire file including the html pages
parse_warc_all = {
         "request" : {"regex" : ["WARC-Date: (.+)", "WARC-Record-ID: (.+)",
                                 "Content-Length: (.+)", "Content-Type:", "WARC-Warcinfo-ID: (.+)",
                                 "WARC-IP-Address: (.+)", "WARC-Target-URI: (.+)", "GET (.+)", "Host: (.+)",  
                                 "Accept-Encoding: (.+)", "User-Agent: (.+)", "Accept: (.+)"],
                      "names" : ["WARC-Date", "WARC-Record-ID",
                                 "Content-Length", "Content-Type", "WARC-Warcinfo-ID",
                                 "WARC-IP-Address", "WARC-Target-URI", "GET", "Host",
                                 "Accept-Encoding", "User-Agent", "Accept"],
                      "instr" : {}
                     },
            
         "response": {"regex" : ["WARC-Date: (.+)", "WARC-Record-ID: (.+)", "Content-Length: (.+)", "Content-Type: (.+)", 
                                 "WARC-Warcinfo-ID: (.+)", "WARC-Concurrent-To: (.+)", "WARC-IP-Address: (.+)",
                                 "WARC-Target-URI: (.+)",
                                 "WARC-Payload-Digest: (.+)", "WARC-Block-Digest: (.+)",
                                 "^HTTP/","^<!DOCTYPE .+>"
                                 ],
                      "names" : ["WARC-Date", "WARC-Record-ID", "Content-Length", "Content-Type",
                                 "WARC-Warcinfo-ID", "WARC-Concurrent-To", "WARC-IP-Address", "WARC-Target-URI",
                                 "WARC-Payload-Digest", "WARC-Block-Digest", 
                                 "HTML-header", "HTML"
                                 ],
                      "instr" : {"HTML-header" : get_html_header, "HTML" : get_html}


                      },
         "metadata" : {"regex" : ["^WARC-Date: (.+)", "^WARC-Record-ID: (.+)",
                                  "^Content-Length: (.+)",
                                  "^Content-Type: (.+)", "^WARC-Warcinfo-ID: (.+)", "^WARC-Concurrent-To: (.+)",
                                  "^WARC-Target-URI: (.+)", "^fetchTimeMs: (.+)"],
                       "names" : ["WARC-Date", "WARC-Record-ID", 
                                  "Content-Length",
                                  "Content-Type", "WARC-Warcinfo-ID", "WARC-Concurrent-To",
                                  "WARC-Target-URI", "fetchTimeMs"],
                       "instr" : {}
                      },
                     
         "warcinfo": {"regex" : ["^WARC-Date: (.+)", "^WARC-Record-ID: (.+)", "^Content-Length: (.+)",
                                 "^Content-Type: (.+)", 
                                 "^WARC-Filename: (.+)", "^robots: (.+)", "^hostname: (.+)", "^software: (.+)",
                                 "^isPartOf: (.+)",
                                 "^operator: (.+)", "^description: (.+)", "^publisher: (.+)", "^format: (.+)", 
                                 "^conformsTo: (.+)"],
                      "names" : ["WARC-Date", "WARC-Record-ID", "Content-Length","Content-Type",
                                 "WARC-Filename", "robots", "hostname", "software", "isPartOf",
                                 "operator", "description", "publisher", "format", "conformsTo"],
                      "instr" : {}
                     }
}
 

#saves only the ip address and the associated URL
parse_warc_ip_url = {
         "request" : {"regex" : ["WARC-IP-Address: (.+)", "WARC-Target-URI: (.+)"],
                      "names" : ["WARC-IP-Address", "WARC-Target-URI"],
                      "instr" : {"WARC-Target-URI" : get_website_from_url}
                     }

}



#ensures that a parse_dic conforms to the above standerd
def check_parse_dic(parse_dic):
    #must be dictionary
    if type(parse_dic) == type({}):
       for key in parse_dic.keys():
           
           d = parse_dic[key]
           #each type of warc format saved must contain the following keys
           if set(d.keys()) == set(["regex", "names", "instr"]):
              
              reg = d["regex"]
              nam = d["names"]
              ins = d["instr"]

              #regex and names must be lists of equal size and instr must be a dic
              if type(reg) == type(nam) and type(reg) == type([]) and type(ins) == type({}):
                 same_length = len(reg) == len(nam)
                 instrs_have_names = True
                 #each key in instr must be in names
                 for i in ins.keys():
                     instrs_have_names = instrs_have_names and (i in nam)
                 if not (same_length and instrs_have_names):
                    return False
              else:
                 return False
           else:
              return False
       return True 
    else:
       return False
    return ret


def parse_warc(filename, parse_dic):
    #throws error if parse_dic is improper
    if not check_parse_dic(parse_dic):
       raise ValueError('parse_dic was not a valid parse_dic')

    #opens the warc file, ignores non utf-8 encoding
    #these encodings will only apprear in the html of
    #webpages so ignoring them should not present a problem
    f = codecs.open(filename,'r',encoding='utf-8',errors='ignore')
    
    #list of warc intervals to be returned
    ret = []
    #current interval, dictionary that will be added to ret
    interval = None
    #regex of warc type currently being parced
    rexs = None
    #names of warc type currently being parced
    names = None
    #instrs of warc type currently being parced
    instrs = None
    #position of next regex to be matched
    i = -1
    #next regex we will attempt to match to the next line
    next_regex = WARC_TYPE_REGEX

    #this function is called when an interval is complete
    #and the parcer will being searching for the next 
    #interval to parce
    def reset():
        #gets nonlocal variables to be modified
        nonlocal rexs, names, i, next_regex, interval, instrs
        #returns them to default position
        rexs = None
        names = None
        i = -1
        next_regex = WARC_TYPE_REGEX
        interval = None
        instrs = None

    #this fuction stores a value with its name in the interval
    def store_val(val, instr):
        nonlocal interval, names, i, f
                
        interval[names[i]] = instr(val, f)

    line = f.readline()

    while line:
          
          line = line.strip()
          
          #at the begining of each warc statment
          #the parcer is reset to make new interval
          if line == NEW_WARC_STATMENT:
             reset()
          
          #if a line is blank goto the next line
          elif line == '':
             pass

          else:
             #attempt to match next_regex to line
             val = regex.findall(next_regex, line)

             #if it does not match continue searching at next line
             if not val:
                pass
            
             #if the line specifies the type of warc object
             #set up parcer to recored this type into interval
             elif next_regex == WARC_TYPE_REGEX:
                warc_type = val[0]
                #if interval is already recording an inerval log the colition
                #and reset
                if interval != None:
                   print("overriding current interval")
                   reset()
                #if warc_type is being recored set up parcer to recored this type into interval
                if warc_type in parse_dic.keys():
                   ty = parse_dic[warc_type]
                   #adds a reset to original state
                   rexs = ty["regex"]
                   names = ty["names"]
                   instrs = ty["instr"]
                   i = 0
                   next_regex = rexs[i]
                   interval = {'WARC-type' : warc_type}
             else:
                  #if no instruction is specified use defualt_intruction
                  instr = defualt_intruction

                  #if one is specified use it
                  if names[i] in instrs.keys():
                     instr = instrs[names[i]]

                  #if this is the last item to recored
                  #recored it, add interval to ret and reset
                  if i == len(names)-1:
                     store_val(val, instr)
                     ret.append(interval)
                     reset() 
                  else:
                     #if not the last item recored it and
                     #increment which regex to seach for next
                     store_val(val, instr)
                     i += 1
                     next_regex = rexs[i]

          #continue onto next line
          line = f.readline()
    f.close()
    return ret     
          

#produces a dictionary that maps ip's to all the associated urls
#in this file
def ip_url_from_warc(file):
    #parses the file according to parse_warc_ip_url
    ip_urls = parse_warc(file, parse_warc_ip_url)

    ret = {}
    #for each recored ip url pair
    for i in ip_urls:
        #if the ip is already in ret add this url to the ip
        if i['WARC-IP-Address'] in ret.keys():
           ret[i['WARC-IP-Address']].add(i['WARC-Target-URI'])
        else:
           #if the ip is not in ret associate a list with this url with the ip
           ret[i['WARC-IP-Address']] = set([i['WARC-Target-URI']])
    
    #makes ret json sterilisable
    for i in ret.keys():
        ret[i] = list(ret[i])
    return ret

#for testing purpuses
#res = parse_warc('sub_test_3', parse_warc_ip_url)
#print(len(res))
#print(res[0])
#for i in res:
 #   print(i['WARC-type'])

#print(ip_url_from_warc('sub_test_3'))

import warc
from urlparse import urlparse

def parse_uri(uri):
    parse = urlparse(uri)
    return parse.netloc

def get_ip_url_dict(file):
    f = warc.WARCFile(fileobj=file) 
    
    print("making dictionary")
   
    ret = {}
    i = 0
    j = 0
    for record in f:
        if record.type == 'response':
           ip = record['WARC-IP-Address']
           uri = parse_uri(record['WARC-Target-URI'])

           if ip in ret:
               ret[ip].add(uri)
           else:
               ret[ip] = set([uri])

           if i > 100:
              print j
              j += 1
              i = 0
           else:
              i += 1

    f.close()
    return ret.items()

#!/usr/bin/env python3
"""Read-only Linux x86_64 BPF inventory and narrow no-redirection admission.

Trusted running kernel and no noncooperating privileged writes are assumptions.
No program/map creation, update, attachment, pinning or execution is exposed.
"""
import ctypes
import errno
import hashlib
import json
import os
import re
import struct

def require(value):
    if not value:
        raise ValueError('BPF recovery qualification rejected')


def read_kernel(path,limit):
    with open(path,'rb') as handle:raw=handle.read(limit+1)
    require(len(raw)<=limit)
    return raw


def observe_once():
    """Read-only Linux x86_64 network bypass metadata; no loading/attachment calls."""
    require(os.uname().sysname == 'Linux' and os.uname().machine == 'x86_64' and (ctypes.sizeof(ctypes.c_void_p) == 8))
    require(os.geteuid() == 0)

    class Ident(ctypes.Structure):
        _fields_ = [('start', ctypes.c_uint32), ('next', ctypes.c_uint32), ('flags', ctypes.c_uint32)]

    class Info(ctypes.Structure):
        _fields_ = [('type', ctypes.c_uint32), ('id', ctypes.c_uint32), ('tag', ctypes.c_ubyte * 8), ('jitedLength', ctypes.c_uint32), ('translatedLength', ctypes.c_uint32), ('jited', ctypes.c_uint64), ('translated', ctypes.c_uint64), ('loadedAt', ctypes.c_uint64), ('uid', ctypes.c_uint32), ('mapCount', ctypes.c_uint32), ('maps', ctypes.c_uint64), ('name', ctypes.c_char * 16), ('ifindex', ctypes.c_uint32), ('flags', ctypes.c_uint32), ('netnsDevice', ctypes.c_uint64), ('netnsInode', ctypes.c_uint64)]

    class Query(ctypes.Structure):
        _fields_ = [('fd', ctypes.c_uint32), ('length', ctypes.c_uint32), ('address', ctypes.c_uint64)]
    libc = ctypes.CDLL(None, use_errno=True)
    libc.syscall.restype = ctypes.c_long
    require(ctypes.sizeof(Ident) == 12 and ctypes.sizeof(Info) == 104 and (ctypes.sizeof(Query) == 16))

    def bpf(command, attr):
        require(command in (1, 11, 13, 14, 15, 19, 30, 31))
        result = libc.syscall(ctypes.c_long(321), ctypes.c_int(command), ctypes.byref(attr), ctypes.c_uint(ctypes.sizeof(attr)))
        if result < 0:
            raise OSError(ctypes.get_errno(), 'BPF metadata read failed')
        return result

    class MapInfo(ctypes.Structure):
        _fields_ = [('type', ctypes.c_uint32), ('id', ctypes.c_uint32), ('keySize', ctypes.c_uint32), ('valueSize', ctypes.c_uint32), ('maxEntries', ctypes.c_uint32), ('flags', ctypes.c_uint32)]

    class BtfInfo(ctypes.Structure):
        _fields_ = [('btf', ctypes.c_uint64), ('size', ctypes.c_uint32), ('id', ctypes.c_uint32), ('name', ctypes.c_uint64), ('nameLength', ctypes.c_uint32), ('kernel', ctypes.c_uint32)]

    class Element(ctypes.Structure):
        _fields_ = [('fd', ctypes.c_uint32), ('pad', ctypes.c_uint32), ('key', ctypes.c_uint64), ('value', ctypes.c_uint64), ('flags', ctypes.c_uint64)]
    programs = []
    fds = []
    last = 0
    maps = {}
    bpfLinks = []
    try:
        while True:
            ident = Ident(last, 0, 0)
            try:
                bpf(11, ident)
            except OSError as e:
                if e.errno == errno.ENOENT:
                    break
                raise
            require(ident.next > last and len(programs) < 256)
            last = ident.next
            fd = bpf(13, Ident(last, 0, 0))
            fds.append(fd)
            info = Info()
            bpf(15, Query(fd, ctypes.sizeof(info), ctypes.addressof(info)))
            require(info.id == last and info.type > 0)
            require(0 < info.translatedLength <= 1024 ** 2 and info.translatedLength % 8 == 0)
            size = info.translatedLength
            count = info.mapCount
            require(count <= 64)
            map_ids = (ctypes.c_uint32 * count)()
            code = ctypes.create_string_buffer(size)
            info = Info()
            info.translatedLength = size
            info.translated = ctypes.addressof(code)
            info.mapCount = count
            info.maps = ctypes.addressof(map_ids)
            bpf(15, Query(fd, ctypes.sizeof(info), ctypes.addressof(info)))
            require(info.translatedLength == size and info.mapCount==count and info.id==last)
            instructions = [list(struct.unpack('<BBhi', code.raw[i:i + 8])) for i in range(0, size, 8)]
            programs.append({'mapIds': list(map_ids), 'instructions': instructions, 'translatedSha256': hashlib.sha256(code.raw).hexdigest(), 'id': info.id, 'type': info.type, 'name': info.name.decode('ascii'), 'tag': bytes(info.tag).hex(), 'createdByUid': info.uid, 'translatedBytes': info.translatedLength, 'ifindex': info.ifindex, 'netnsDevice': info.netnsDevice, 'netnsInode': info.netnsInode})
        for map_id in sorted(set((mid for p in programs for mid in p['mapIds']))):
            fd = bpf(14, Ident(map_id, 0, 0))
            fds.append(fd)
            mi = MapInfo()
            bpf(15, Query(fd, ctypes.sizeof(mi), ctypes.addressof(mi)))
            row = {key: getattr(mi, key) for key, _ in mi._fields_}
            require(row['id'] == map_id)
            if mi.type == 3:
                require(mi.keySize == 4 and mi.valueSize == 4 and (mi.maxEntries <= 1024))
                populated = []
                for index in range(mi.maxEntries):
                    key = ctypes.c_uint32(index)
                    value = ctypes.c_uint32()
                    try:
                        bpf(1, Element(fd, 0, ctypes.addressof(key), ctypes.addressof(value), 0))
                    except OSError as e:
                        if e.errno == errno.ENOENT:
                            continue
                        raise
                    populated.append({'slot': index, 'programId': value.value})
                row['populatedSlots'] = populated
                row['slotsRead'] = mi.maxEntries
            maps[str(map_id)] = row
        last = 0
        while True:
            ident = Ident(last, 0, 0)
            try:
                bpf(31, ident)
            except OSError as e:
                if e.errno == errno.ENOENT:
                    break
                raise
            require(ident.next > last and len(bpfLinks) < 256)
            last = ident.next
            fd = bpf(30, Ident(last, 0, 0))
            fds.append(fd)
            buf = ctypes.create_string_buffer(64)
            bpf(15, Query(fd, 64, ctypes.addressof(buf)))
            kind, lid, prog = struct.unpack_from('<III', buf.raw)
            require(lid==last)
            row = {'type': kind, 'id': lid, 'programId': prog}
            if kind == 2:
                row.update(zip(('attachType', 'targetObjectId', 'targetBtfId'), struct.unpack_from('<III', buf.raw, 16)))
            bpfLinks.append(row)
        btfObjects = {}
        for object_id in sorted(set((x['targetObjectId'] for x in bpfLinks if x['type'] == 2))):
            fd = bpf(19, Ident(object_id, 0, 0))
            fds.append(fd)
            bi = BtfInfo()
            bpf(15, Query(fd, ctypes.sizeof(bi), ctypes.addressof(bi)))
            require(0 < bi.size <= 32 * 1024 ** 2 and 0 < bi.nameLength <= 64)
            size = bi.size
            name_length = bi.nameLength
            data = ctypes.create_string_buffer(size)
            name = ctypes.create_string_buffer(name_length + 1)
            bi = BtfInfo(ctypes.addressof(data), size, 0, ctypes.addressof(name), name_length + 1, 0)
            bpf(15, Query(fd, ctypes.sizeof(bi), ctypes.addressof(bi)))
            require(bi.size == size and bi.id == object_id)
            btfObjects[str(object_id)] = {'id': bi.id, 'name': name.value.decode('ascii'), 'kernel': bi.kernel, 'sha256': hashlib.sha256(data.raw).hexdigest()}
    finally:
        for fd in fds:
            os.close(fd)
    symbols = {}
    for line in read_kernel('/proc/kallsyms',64*1024**2).decode().splitlines():
        fields = line.split()
        if len(fields) >= 3:
            symbols.setdefault(int(fields[0], 16), []).append(fields[2])
    base = [address for address, names in symbols.items() if '__bpf_call_base' in names]
    require(len(base) == 1 and base[0] > 0)
    for p in programs:
        p['resolvedCalls'] = [{'index': i, 'immediate':instruction[3], 'symbols': symbols.get(base[0] + instruction[3], [])} for i, instruction in enumerate(p['instructions']) if instruction[0] == 133 and instruction[1] == 0 and (instruction[3] != 12)]
    btf = read_kernel('/sys/kernel/btf/vmlinux',32*1024**2)
    require(len(btf) <= 32 * 1024 ** 2)
    magic, version, flags, hdr, typOff, typLen, strOff, strLen = struct.unpack_from('<HBBIIIII', btf)
    require(magic == 60319 and version == 1 and flags == 0 and hdr>=24
            and hdr+typOff+typLen<=len(btf) and hdr+strOff+strLen<=len(btf))
    names = btf[hdr + strOff:hdr + strOff + strLen]
    position = hdr + typOff
    end = position + typLen
    typeid = 0
    targets = {x.get('targetBtfId') for x in bpfLinks if btfObjects.get(str(x.get('targetObjectId')), {}).get('name') == 'vmlinux'}
    resolved = {}
    while position < end:
        no, info, kind_type = struct.unpack_from('<III', btf, position)
        kind = info >> 24 & 31
        vlen = info & 65535
        typeid += 1
        extra = {1: 4, 2: 0, 3: 12, 4: vlen * 12, 5: vlen * 12, 6: vlen * 8, 7: 0, 8: 0, 9: 0, 10: 0, 11: 0, 12: 0, 13: vlen * 8, 14: 4, 15: vlen * 12, 16: 0, 17: 4, 18: 0, 19: vlen * 12}[kind]
        if typeid in targets:
            require(no < len(names))
            stop = names.index(0, no)
            resolved[typeid] = {'kind': kind, 'name': names[no:stop].decode('ascii')}
        position += 12 + extra
    require(position == end)
    for row in bpfLinks:
        if btfObjects.get(str(row.get('targetObjectId')), {}).get('name') == 'vmlinux':
            require(btfObjects[str(row['targetObjectId'])]['kernel'] == 1 and btfObjects[str(row['targetObjectId'])]['sha256'] == hashlib.sha256(btf).hexdigest())
            row['targetBtf'] = resolved.get(row['targetBtfId'])
    return {'schemaVersion': 1, 'enumerationComplete':True,'kernelRelease': os.uname().release, 'vmlinuxBtfSha256': hashlib.sha256(btf).hexdigest(), 'programs': programs, 'maps': maps, 'links': bpfLinks, 'btfObjects': btfObjects}


def pure_verdict(program):
    """Conservative observed subset: reads/ALU/forward jumps/exit only.

    Kernel verification remains trusted. No stores, calls, atomics, two-slot
    loads, unsupported encodings, or backward branches enter this subset.
    """
    instructions=program['instructions']
    if program['type'] not in (8,15) or program['mapIds'] or program['resolvedCalls']:return False
    for index,(op,regs,offset,imm) in enumerate(instructions):
        if op not in (5,68,84,85,93,97,105,116,149,180,183,188,191):return False
        if (regs & 15)>10 or (regs>>4)>10:return False
        if op in (5,85,93) and not (0<=offset and index+1+offset<len(instructions)):return False
        if op==5 and (regs or imm):return False
        if op==85 and regs>>4:return False
        if op==93 and imm:return False
        if op in (97,105) and (imm or (regs & 15)==10):return False
        if op in (68,84,116,180,183) and (offset or regs>>4 or (regs & 15)==10):return False
        if op in (188,191) and (offset or imm or (regs & 15)==10):return False
        if op==149 and (regs,offset,imm)!=(0,0,0):return False
    return instructions[-1]==[149,0,0,0]


def address_verdict(program,maps):
    # systemd255 IPv4 allow-list + deny-any shape. Only map identity and the
    # proven running-kernel call relocations vary; stack destinations do not.
    if program['type']!=8 or len(program['mapIds'])!=1:return False
    map_id=program['mapIds'][0];row=maps.get(str(map_id),{})
    if row!={'type':11,'id':map_id,'keySize':8,'valueSize':8,'maxEntries':1,'flags':1}:return False
    code=program['instructions']
    if len(code)!=23:return False
    calls=[{'index':9,'immediate':code[9][3],'symbols':['bpf_skb_load_bytes']},
           {'index':15,'immediate':code[15][3],'symbols':['trie_lookup_elem']}]
    if program['resolvedCalls']!=calls:return False
    expected=[[191,22,0,0],[105,103,180,0],[180,8,0,0],[85,7,14,8],
              [191,97,0,0],[180,2,0,16],[191,163,0,0],[7,3,0,-4],
              [180,4,0,4],[133,0,0,code[9][3]],[24,17,0,map_id],[0,0,0,0],
              [191,162,0,0],[7,2,0,-8],[98,2,0,32],[133,0,0,code[15][3]],
              [21,0,1,0],[68,8,0,1],[68,8,0,2],[183,0,0,1],[85,8,1,2],
              [183,0,0,0],[149,0,0,0]]
    return code==expected or code==[row if i!=5 else [180,2,0,12] for i,row in enumerate(expected)]


def hid_noop(program,snapshot):
    if program['type']!=26 or len(program['mapIds'])!=1 or program['resolvedCalls']:return False
    map_id=program['mapIds'][0]
    if program['instructions']!=[[121,18,0,0],[97,35,0,0],[24,18,0,map_id],
                                 [0,0,0,0],[133,0,0,12],[183,0,0,0],[149,0,0,0]]:return False
    if snapshot['maps'].get(str(map_id))!={'type':3,'id':map_id,'keySize':4,'valueSize':4,
            'maxEntries':1024,'flags':0,'slotsRead':1024,'populatedSlots':[]}:return False
    links=[row for row in snapshot['links'] if row['programId']==program['id']]
    if len(links)!=1:return False
    link=links[0]
    if (link['type']!=2 or link.get('attachType')!=26
            or link.get('targetBtf')!={'kind':12,'name':'__hid_bpf_tail_call'}):return False
    obj=snapshot['btfObjects'].get(str(link['targetObjectId']),{})
    return obj=={'id':link['targetObjectId'],'name':'vmlinux','kernel':1,
                'sha256':snapshot['vmlinuxBtfSha256']}


def check_inventory(snapshot):
    require(snapshot['schemaVersion']==1 and snapshot['enumerationComplete'] is True)
    programs=snapshot['programs'];require(isinstance(programs,list) and len(programs)<=256)
    ids=[p['id'] for p in programs]
    require(all(type(value) is int and value>0 for value in ids) and len(ids)==len(set(ids)))
    counts={'pureVerdict':0,'addressVerdict':0,'hidEmptyTailCall':0}
    map_ids=set()
    for program in programs:
        code=program['instructions']
        require(isinstance(code,list) and 0<len(code)<=131072)
        for row in code:
            require(isinstance(row,list) and len(row)==4 and all(type(value) is int for value in row)
                    and 0<=row[0]<256 and 0<=row[1]<256 and -32768<=row[2]<32768 and -2**31<=row[3]<2**31)
        raw=b''.join(struct.pack('<BBhi',*row) for row in code)
        require(program['translatedBytes']==len(raw) and program['translatedSha256']==hashlib.sha256(raw).hexdigest()
                and program['createdByUid']==0 and program['ifindex']==0
                and program['netnsDevice']==0 and program['netnsInode']==0)
        require(isinstance(program['mapIds'],list) and len(program['mapIds'])<=64
                and all(type(mid) is int and 0<mid<2**32 for mid in program['mapIds'])
                and len(set(program['mapIds']))==len(program['mapIds']))
        map_ids.update(program['mapIds'])
        if pure_verdict(program):counts['pureVerdict']+=1
        elif address_verdict(program,snapshot['maps']):counts['addressVerdict']+=1
        elif hid_noop(program,snapshot):counts['hidEmptyTailCall']+=1
        else:require(False)
    require(set(snapshot['maps'])=={str(value) for value in map_ids})
    # No unclassified link/attachment can hide behind an admitted program name.
    hid_ids={p['id'] for p in programs if p['type']==26}
    require(len(snapshot['links'])==len(hid_ids)
            and {row['programId'] for row in snapshot['links']}==hid_ids
            and len({row['id'] for row in snapshot['links']})==len(snapshot['links']))
    return counts


def observe():
    before=observe_once();check_inventory(before)
    require(observe_once()==before)
    return before


def admit(snapshot,qualification):
    """Caller establishes provenance; hexadecimal evidence references are not approval."""
    require(set(qualification)=={'kernelRelease','vmlinuxBtfSha256','sourceRevision','inspectionEvidenceSha256'})
    require(qualification['kernelRelease']==snapshot['kernelRelease']
            and qualification['vmlinuxBtfSha256']==snapshot['vmlinuxBtfSha256'])
    for key,length in (('vmlinuxBtfSha256',64),('sourceRevision',40),('inspectionEvidenceSha256',64)):
        require(isinstance(qualification[key],str) and re.fullmatch('[a-f0-9]{'+str(length)+'}',qualification[key]))
    counts=check_inventory(snapshot)
    return {'schemaVersion':1,'bpfProgramsWithinQualifiedNoRedirectionSubset':True,
            'counts':counts,'qualification':qualification,'hostBypassAdmissionVerified':False}

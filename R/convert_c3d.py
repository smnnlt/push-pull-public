import c3d

def convert(filename, save_in_top_folders = True):
    input = open(filename, 'rb')
    outname = filename.replace('.c3d', '.csv')
    if save_in_top_folders:
        parts = outname.split("/")
        outname1 = "/".join(parts[:-2]) + '/mc/' + parts[-1]
        outname2 = "/".join(parts[:-2]) + '/emg/' + parts[-1]
        output = open(outname1, 'w')
        output2 = open(outname2, 'w')
    else:
        output = open(filename.replace('.c3d', '.csv'), 'w')
        output2 = open(filename.replace('.c3d', '_ext.csv'), 'w')
    try:
        data = c3d.Reader(input)
        # motion data header
        point_name = [i.strip() for i in data.point_labels] # remove whitespace
        # names = [a[a.index(':')+1:] for a in point_name] # remove first part of label
        x = [val for val in point_name for _ in range(3)] # repeat labels
        xyz = ['-x', '-y', '-z'] * len(point_name)
        header = [m+n for m,n in zip(x, xyz)]
        header.insert(0, 'frame') # add first label
        print(*header, sep=',', end='\n',file=output)
        # external parameter header
        params = [i.strip() for i in data.analog_labels]
        params.insert(0, 'frame')
        print(*params, sep=',', end='\n',file=output2)
        for frame_no, points, analog in data.read_frames(copy=False):
            # motion data
            fields = [frame_no]
            for x, y, z, err, cam in points:
                fields.append(str(x))
                fields.append(str(y))
                fields.append(str(z))
            print(*fields, sep=',', end='\n', file=output)
            # external data
            for t in range(data.analog_per_frame):
                row = [str(frame_no) + '_' + str(t + 1)]
                for m in range(data.analog_used):
                    row.append(str(analog[m][t]))
                print(*row, sep=',', end='\n', file=output2)
    finally:
        input.close()
        output.close()
        output2.close()
        


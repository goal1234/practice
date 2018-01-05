# 设定工作目录

import os
path = r'E:\just language\reports\data'
os.getcwd()
os.chdir(path)

out_path = r'E:\just language\reports\data\out'
is_exist = os.path.exists(out_path)

if not is_exist:
    os.makedirs(out_path)

# 是否存在文件
os.path.isfile(filename)

# 列出目录和文件
os.listdir(path)

# 返回当前目录
os.curdir()

# 是不是一个目录
os.path.isdir(path)

# 文件夹大小
os.path.getsize(path)

# 分隔目录
os.path.split(path)

# 绝对路径
os.path.abspath(path)

# 分离文件名和拓展名 
os.path.splitext()

# 连接
os.path.join(path, name)

# 返回文件名
os.path.basename(path)

# 返回文件路径
os.path.dirname(path)

# 遍历文件目录
import os, sys

def listdir(dir, file):
    file.write(dir + '\n)
    filenum = 0
    lst = os.listdir(dir):
    for line in lst:
        filepath = os.path.join(dir, line)
        if os.path.isdir(filepath):
            myfile.write(" ' + line + '\\' + '\n)
            for li in os.listdir(filepath):
                myfile.write(" '+li+'\n)
                filenum = filenum + 1
        elif os.path:
            myfile.write("' + line +'\n)
            filenum = filenum + 1
    myfile.write('all the file num is' + str(filenum))

dir = input('please input the path:')
myfile = open('list.txt','w')
listdir(dir, myfile)


# ---
import os
def walk_dir(dir,fileinfo,topdown=True):
    for root, dirs, files in os.walk(dir, topdown):
        for name in files:
            print(os.path.join(name))
            fileinfo.write(os.path.join(root,name) + '\n')
        for name in dirs:
            print(os.path.join(name))
            fileinfo.write('  ' + os.path.join(root,name) + '\n')
dir = raw_input('please input the path:')
fileinfo = open('list.txt','w')
walk_dir(dir,fileinfo)

# ---
import os
import os.path
"""获取指定目录及其子目录下的 py 文件路径说明：l 用于存储找到的 py 文件路径 get_py 函数，递归查找并存储 py 文件路径于 l"""
l = []
def get_py(path,l):
    fileList = os.listdir(path)   #获取path目录下所有文件
    for filename in fileList:
        pathTmp = os.path.join(path,filename)   #获取path与filename组合后的路径
        if os.path.isdir(pathTmp):   #如果是目录
            get_py(pathTmp,l)        #则递归查找
        elif filename[-3:].upper()=='.PY':   #不是目录,则比较后缀名
            l.append(pathTmp)
path = input('请输入路径:').strip()
get_py(path,l)
print('在%s目录及其子目录下找到%d个py文件\n分别为：\n'%(path,len(l)))
for filepath in l:
    print(filepath+'\n')

# ---显示所有视频格式文件，mp4，avi，rmvb
import os

def search_file(start_dir, target) :
    os.chdir(start_dir)
    
    for each_file in os.listdir(os.curdir) :
        ext = os.path.splitext(each_file)[1]
        if ext in target :
            vedio_list.append(os.getcwd() + os.sep + each_file + os.linesep) 
        if os.path.isdir(each_file) :
            search_file(each_file, target) # 递归调用
            os.chdir(os.pardir) # 递归调用后切记返回上一层目录

start_dir = input('请输入待查找的初始目录：')
program_dir = os.getcwd()

target = ['.mp4', '.avi', '.rmvb']
vedio_list = []

search_file(start_dir, target)

f = open(program_dir + os.sep + 'vedioList.txt', 'w')
f.writelines(vedio_list)
f.close()

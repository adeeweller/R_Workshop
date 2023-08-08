# in terminal
# cd, .., cd\, dir, makedir "make_file.txt", ren Folder NewFolderName, copy, del file, help

# PS C:\Users\adeew> cd OneDrive\Documents\fall_2022

# exit()

# getting working directories
import os

cwd = os.getcwd()
print("Current working directory: {0}".format(cwd))

# changing directories (if necessary)
if not cwd == "C:/Users/adeew/OneDrive/Documents/fall_2022":
    os.chdir("C:/Users/adeew/OneDrive/Documents/fall_2022")

# check work
cwd = os.getcwd()
print('getcwd:      ', os.getcwd())

# list files
dir_list = os.listdir()

print("files and directories in '", cwd, "' :", dir_list)

# for loop
for file in dir_list:
    if file.endswith(".docx"):
        print(file)



# writing and appending files

# writing a new file
f = open("new_file.txt", "w")

f

f.write("Now we can begin our file.")

print(f)
f.close()

# check if it's been created properly
dir_list = os.listdir()
print("files and directories in '", cwd, "' :", dir_list)

# append file (add to it)
f = open("new_file.txt", "a")
f.write("\n Here is another line of critical information.")
f.close()

# open and read
f = open('new_file.txt', 'r')
print(f.read())

# try the append again!
# try it in the write mode
# what happens?

# to create an empty file
# f = open("myfile.txt", "x")

# who can open and read this file first?
f = open('sample.txt', 'r')
print(f.read())




# print('abspath:     ', os.path.abspath(__file__))
# print('abs dirname: ', os.path.dirname(os.path.abspath(__file__)))
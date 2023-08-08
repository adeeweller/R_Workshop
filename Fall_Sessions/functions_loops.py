# writing functions

def hello():
    print('Hello world!')

hello()

# defining parameters
def hello(name):
    print('Hello, ' + name)

hello('Alice')


# ifelse statements and return

import random

def Magic8(answerNumber):
    if answerNumber == 1:
           return 'It is certain'
    elif answerNumber == 2:
           return 'It is decidedly so'
    elif answerNumber == 3:
           return 'Yes'
    elif answerNumber == 4:
           return 'Reply hazy try again'
    elif answerNumber == 5:
           return 'Ask again later'
    elif answerNumber == 6:
           return 'Concentrate and ask again'
    elif answerNumber == 7:
           return 'My reply is no'
    elif answerNumber == 8:
           return 'Outlook not so good'
    elif answerNumber == 9:
           return 'Very doubtful'

r = random.randint(1, 9)
fortune = Magic8(r)

print(fortune)



# lists and loops

animals = ['dog', 'cat', 'hamster', 'fish']

animals[0]

animals[-1]

len(animals)


mammals = []
counter = 0

for category in animals:
    if category == 'dog':
        mammals.append(category)
    elif category == 'cat':
        mammals.append(category)
    elif category == 'hamster':
        mammals.append(category)
    counter += 1
    
counter


print(mammals)
print(len(mammals))




# make a list of different animals

# write a loop where if the animal is a mammal, write out a list and get a count of the examples
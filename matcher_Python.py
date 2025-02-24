class Regex(object):
    def __init__(self, empty):
        self.empty = empty
        self.marked = False

    def reset(self):
        """Reset all marks in the regular expression."""
        self.marked = False

    def shift(self, c, mark):
        """Shift the mark from left to right, matching character c."""
        
        marked = self._shift(c, mark)
        self.marked = marked
        return marked

class Char(Regex):
    def __init__(self, c):
        super().__init__(False)
        self.c = c

    def _shift(self, c, mark):
        return mark and c == self.c
    
    def __str__(self):
        return " Char: " + str(self.c) + " , Char mark: " + str(self.marked)

class Epsilon(Regex):
    def __init__(self):
        super().__init__(empty=True)

    def _shift(self, c, mark):
        return False

class Binary(Regex):
    def __init__(self, left, right, empty):
        super().__init__(empty)
        self.left = left
        self.right = right

    def reset(self):
        self.left.reset()
        self.right.reset()
        super().reset()

class Alternative(Binary):
    def __init__(self, left, right):
        empty = left.empty or right.empty
        super().__init__(left, right, empty)

    def _shift(self, c, mark):
        marked_left = self.left.shift(c, mark)
        marked_right = self.right.shift(c, mark)
        return marked_left or marked_right
    
    def __str__(self):
        return " ALT: Left " + str(self.left) + " ,right "+str(self.left)+",ALT mark: " + str(self.marked)

class Sequence(Binary):
    def __init__(self, left, right):
        empty = left.empty and right.empty
        super().__init__(left, right, empty)

    def _shift(self, c, mark):
        old_marked_left = self.left.marked
        marked_left = self.left.shift(c, mark)
        marked_right = self.right.shift(
            c, old_marked_left or (mark and self.left.empty))
        return (marked_left and self.right.empty) or marked_right
    
    def __str__(self):
        return " SEQ: Left" + str(self.left) + " ,right "+str(self.left)+", SEQ mark: " + str(self.marked)

class Repetition(Regex):
    def __init__(self, re):
        super().__init__(True)
        self.re = re

    def _shift(self, c, mark):
        return self.re.shift(c, mark or self.marked)

    def reset(self):
        self.re.reset()
        super().reset()

    def __str__(self):
        return " Repitition: mark: " + str(self.marked)

def match(re, s):
    if not s:
        return re.empty
    result = re.shift(s[0], True)
    print(re)
    for c in s[1:]:
        result = re.shift(c, False)
    print("------after loop ------")
    print(re)
    re.reset()
    return result


reg= Alternative(Alternative(Char('c'), Char('a'))  , Char('d'))
print(match(reg,"dd"))


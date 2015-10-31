
# TODO Documentation
# TODO Implement a version where there is only one directory value
# TODO Implement 'broken' command

# Constants for Content.type:
DIR   = ' Dir'
FILE  = 'File'
EMPTY = 'Empty'


class Content:
    """Represents some content (element of set V) at a given path in the filesystem
    
    Private properties:
        type (enum): DIR or FILE or EMPTY
        value (str): an arbitrary string representing e.g. file contents and metadata
        
    """

    def __init__(self, type=EMPTY, value='Unknown'):
        """Constructor
        
        Args:
            type (Optional[enum]):
            value (Optional[str]):
        
        """
        self.type = type
        self.value = value
        
    def clone(self):
        """Returns a deep clone of the object."""
        return self.__class__(self.type, self.value)

    def info(self, addvalue=True):
        """Returns human-readable information about the object.
        
        Args:
            addvalue (Optional[bool]): whether to add the value to the output
            
        Returns:
            str
            
        """
        r = self.type
        if not self.isEmpty() and addvalue: r += "(" + self.value + ")"
        return r
        
    def isSame(self, content):
        """Returns whether the object is the same as another content object."""
        return (self.type == content.type and self.value == content.value)
        
    def getType(self):
        return self.type
    
    def getValue(self):
        if self.isEmpty(): return 'Unknown'
        return self.value
        
    def isDir(self):
        """Returns whether the type of the object is 'diretory'."""
        return (self.type == DIR)

    def isFile(self):
        """Returns whether the type of the object is 'file'."""
        return (self.type == FILE)

    def isEmpty(self):
        """Returns whether the type of the object is 'empty'."""
        return (self.type == EMPTY)


def ContentFactory(value='Unknown'):
    """Generates all possible contents.
    
    Args:
        value (Optional[str]): the value of the content objects
        
    Yields:
        All possible content objects
        
    """
    yield Content(EMPTY)
    yield Content(FILE, value)
    yield Content(DIR, value)


class Node:
    """Represents a path in the filesystem and information about its environment

    Private properties:    
        has_parent (bool): whether the node has a parent (i.e. there is a directory at the parent path)
        content (Content): a content object
        has_child (bool): whether the node has a child (i.e. one of the child paths is not empty)
        broken (None|str): None if the filesystem is not broken at this node;
            the reason for being broken otherwise

    """

    def __init__(self, has_parent=False, content=None, has_child=False, broken=False):
        """Constructor.

        Args:
            has_parent (Optional[bool]):
            content (Optional[Content]):
            has_child (Optional[bool]):
            broken (Optional[None|str]):

        """
        self.has_parent = has_parent
        self.content = Content() if content is None else content
        self.has_child = has_child
        self.broken = 'Constructor' if broken else None
        self.checkTreeProperty()
        
    def clone(self):
        """Returns a deep clone of the object."""
        return self.__class__(self.has_parent, self.content.clone(), self.has_child, self.broken)
        
    def info(self, debug=False):
        """Returns a human-readable string describing the object.

        Args:
            debug (Optional[bool]): include extra information if True

        Returns:
            str

        """
        r = []
        if self.isBroken():
            if debug:
                r.append("Broken(" + self.broken + ") ")
            else:
                return "(Broken)"
        if self.has_parent: r.append("o--")
        r.append(self.content.info())
        if self.has_child: r.append("--o")
        return "(" + "".join(r) + ")"
        
    def isSame(self, node):
        """Returns whether the object is the same as another node object."""
        if self.isBroken() and node.isBroken(): return True
        if self.isBroken() or node.isBroken(): return False
        return (self.has_parent == node.has_parent and self.content.isSame(node.content) and self.has_child == node.has_child)

    def isBroken(self):
        """Returns whether the node is marked as broken."""
        return not self.broken is None
        
    def setBroken(self, reason='unknown'):
        """Mark the node as broken.

        Args:
            broken (Optional[str]):

        Returns:
            self

        """
        self.broken = reason
        return self

    def getContent(self):
        return self.content
        
    def setContent(self, content):
        self.content = content
        self.checkTreeProperty()
        return self
        
    def setHasChild(self, v):
        self.has_child = v
        self.checkTreeProperty()
        return self
        
    def setHasParent(self, v):
        self.has_parent = v
        self.checkTreeProperty()
        return self
        
    def checkTreeProperty(self):
        """Mark the node as broken if there is a contradiction between its environment and its contents.

        Returns:
            self

        """
        if not self.content.isEmpty() and not self.has_parent:
            self.broken = 'tree-nonempty-noparent'
        if self.has_child and not self.content.isDir():
            self.broken = 'tree-notdir-haschild'
        return self
        
    def assertDescendant(self):
        """Mark the node as broken if it does not have children.

        Returns:
            self

        """
        # print "assert child on " + self.info()
        if not self.has_child or not self.content.isDir():
            self.broken = 'assert-child'
        return self
        
    def assertNoDescendants(self):
        """Mark the node as broken if it has a child.

        Returns:
            self

        """
        # print "assert no child on " + self.info()
        if self.has_child:
            self.broken = 'assert-no-child'
        return self
        
    def assertParent(self):
        """Mark the node as broken if it has no parent.

        Returns:
            self

        """
        # print "assert parent on " + self.info()
        if not self.has_parent:
            self.broken = 'assert-parent'
        return self
        
    def assertNoParent(self):
        """ Mark the node as broken if it has a parent.

        Returns:
            self

        """
        # print "assert no parent on " + self.info()
        if self.has_parent:
            self.broken = 'assert-no-parent'
        return self


def NodeFactory(contents='Unknown'):
    """Generates all possible nodes.

    Args:
        contents (Optional[str]): the content of the value of the node

    Yields:
        All possible node objects

    """
    yield Node(broken=True)
    for has_parent in [False, True]:
        for content in ContentFactory(contents):
            for has_child in [False, True]:
                node = Node(has_parent, content, has_child)
                if not node.isBroken():
                    yield node


# Constants for Filesystem.rel and CommandPair.rel:
DIRECT_PARENT      = 'DirectParent'      # p2 is the parent of p1
DIRECT_PARENT_ONLY = 'DirectParentOnly'  # p2 is the parent of p1 and p1 is the only child
DIRECT_CHILD       = 'DirectChild'       # p2 is the child of p2
DIRECT_CHILD_ONLY  = 'DirectChildOnly'   # p2 is the child of p1 and p2 is the only child
SEPARATE           = 'Separate'          # all other cases
SAME               = 'Same'              # two paths are the same (used for command pairs)

class Filesystem:
    """Models two paths in a filesystem to simulate the effects of a pair of commands.

    Private properties:    
        p1 (Node): a node object; the filesystem at path p1
        p2 (Node): a node object; the filesystem at path p2
        rel (enum): the relationship between p1 and p2:
            DIRECT_PARENT or DIRECT_PARENT_ONLY or DIRECT_CHILD or DIRECT_CHILD_ONLY or SEPARATE

    """
    
    def __init__(self, p1, p2, rel):
        """ Constructor.

        Args:
            p1 (Node):
            p2 (Node):
            rel (enum):

        """
        self.p1 = p1
        self.p2 = p2
        self.rel = rel
        self.checkTreeProperty()
        
    def info(self, debug=False):
        """Returns a human-readable string describing the object."""
        if self.isBroken() and not debug:
            return "[Broken]"
        if self.rel == SEPARATE:
            return self.p1.info(debug) + " ==x== " + self.p2.info(debug)
        if self.rel == DIRECT_CHILD:
            return self.p1.info(debug) + " ===<> " + self.p2.info(debug)
        if self.rel == DIRECT_CHILD_ONLY:
            return self.p1.info(debug) + " ===>> " + self.p2.info(debug)
        if self.rel == DIRECT_PARENT:
            return self.p2.info(debug) + " ~~~<> " + self.p1.info(debug)
        if self.rel == DIRECT_PARENT_ONLY:
            return self.p2.info(debug) + " ~~~>> " + self.p1.info(debug)
    
    def clone(self):
        """Returns a deep clone of the object."""
        return self.__class__(self.p1.clone(), self.p2.clone(), self.rel)
        
    def isSame(self, fs):
        """Returns whether the object is the same as another filesystem object."""
        if self.isBroken() and fs.isBroken(): return True
        if self.isBroken() or fs.isBroken(): return False
        return (self.p1.isSame(fs.p1) and self.p2.isSame(fs.p2) and self.rel == fs.rel)
        
    def isExtendedBy(self, fs):
        """Returns whether self is the same as another filesystem object
            when self is not broken.
        """
        if self.isBroken(): return True
        if fs.isBroken(): return False
        return (self.p1.isSame(fs.p1) and self.p2.isSame(fs.p2) and self.rel == fs.rel)
    
    def isBroken(self):
        """Returns whether the filesystem is broken."""
        return (self.p1.isBroken() or self.p2.isBroken())
        
    def checkTreeProperty(self):
        """Mark the filesystem as broken if there is a contradiction
            between the environments noted in the nodes an their relationship.
        """
        if self.rel == SEPARATE:
            return self

        if self.rel in [DIRECT_CHILD, DIRECT_CHILD_ONLY]:
            parent = self.p1
            child = self.p2
        else:
            parent = self.p2
            child = self.p1
            
        only = self.rel in [DIRECT_CHILD_ONLY, DIRECT_PARENT_ONLY]
    
        if not child.getContent().isEmpty():
            # print "child not empty"
            parent.assertDescendant()

        if not parent.getContent().isEmpty():
            # print "parent not empty"
            child.assertParent()
        else:
            # print "parent empty"
            child.assertNoParent()

        if only and child.getContent().isEmpty():
            # print "only child and child empty"
            parent.assertNoDescendants()
            
            
    def applyCommand(self, command):
        """Apply a command to the filesystem.

        Note:
            The path to apply the command at is stored in the command.
            This function implements actions based on what the filesystem
            knows: the relationship between the nodes. It updates the
            environments stored in the nodes accordingly.

        Args:
            command (Command): a command object

        Returns:
            self            

        """

        command_path = command.getPath()
        new_content = command.getEnd()

        # We update the environments stored in the other node if needed.
        # The node objects mark themselves as broken after any change
        # if that violates the tree property. This is safe, however,
        # as we apply only a single change to each path.
        if self.rel != SEPARATE:
            if self.rel in [DIRECT_CHILD, DIRECT_CHILD_ONLY]:
                childpath = PATH2
                child = self.p2
                parentpath = PATH1
                parent = self.p1
            else:
                childpath = PATH1
                child = self.p1
                parentpath = PATH2
                parent = self.p2                

            if command_path == childpath:
                
                # If the child will have content, the parent will have a child
                if not new_content.isEmpty():
                    parent.setHasChild(True)
                
                # If the only child is deleted, the parent will have no child
                if self.rel in [DIRECT_CHILD_ONLY, DIRECT_PARENT_ONLY] and new_content.isEmpty():
                    parent.setHasChild(False)
                    
            if command_path == parentpath:
            
                # If the parent is deleted, the child will have no parent
                if new_content.isEmpty():
                    child.setHasParent(False)
                
                # If the parent is created, the child will have a parent
                else:
                    child.setHasParent(True)
  
        # Here the command is always applied to a different path than the one we changed above
        command.applyToNode(self.p1 if command_path == PATH1 else self.p2)
            
        return self
        
    def applySequence(self, sequence):
        """Apply a sequence of commands to the filesystem."""
        sequence.map(lambda x: self.applyCommand(x))
        return self


def FilesystemFactory(rel):
    """Generates all possible filesystems with the given relationship between p1 and p2.

    Args:
        rel (enum): DIRECT_PARENT or DIRECT_PARENT_ONLY or DIRECT_CHILD or DIRECT_CHILD_ONLY or SEPARATE

    Yields:
        possible filesystem objects

    """
    yield Filesystem(Node(broken=True), Node(broken=True), rel)
    for p1_source in NodeFactory('Old1'):
        for p2 in NodeFactory('Old2'):
            fs = Filesystem(p1_source.clone(), p2, rel)  # the constructor may break the p1 node, so we need to clone
            if not fs.isBroken():
                yield fs


# Constants for Command.path:
PATH1 = 'P1'
PATH2 = 'P2'


class Command:
    """Represents a command.

    Private properties:
        path (enum): PATH1 or PATH2
        start (Content): a content object. Its value is disregarded; only the type is used
            to note the type of content the command expects in the filesystem
            before it runs
        end (Content): a content object the command turns the path into
    """
    
    def __init__(self, path, start, end):
        """Constructor.

        Args:
            path (enum):
            start (Content):
            end (Content):

        """
        self.path = path
        self.start = start
        self.end = end
        
    def info(self, debug=False):
        """Returns a human-readable string describing the object."""
        return "{" + self.path + ":" + self.start.info(False) + ">" + self.end.info() + "}"

    def getPath(self):        
        return self.path
        
    def getEnd(self):
        return self.end

    def applyToNode(self, node):
        """Apply the command to a node.

        Args:
            node (Node): a node object

        Returns:
            self

        """
        if node.getContent().getType() != self.start.getType():
            node.setBroken('command-start')
            return self
        node.setContent(self.end)
        return self


def CommandFactory(path, value):
    """Generates all possible commands that uses the given value in its end-content (if applicable).

    Args:
        value (str): an arbitrary string representing the value (e.g. file contents and metadata)

    Yields:
        all possible command objects

    """
    for c1 in ContentFactory('N/A'):
        for c2 in ContentFactory(value):
            yield Command(path, c1, c2)


class Sequence:
    """Represents a sequence of commands.
    
    Private properties:
        commands (list): a list of command objects

    """
    
    def __init__(self, commands):
        self.commands = commands
        
    def info(self, debug=False):
        """Returns a human-readable string describing the object"""
        return "; ".join(map(lambda x: x.info(debug), self.commands))
        
    def clone(self):
        """Returns a shallow clone of the sequence. Commands are not mutable."""
        return self.__class__(self.commands[:])
        
    def getReverse(self):
        """Returns a new object with the sequence reversed."""
        tmp = self.clone()
        tmp.commands.reverse()
        return tmp
        
    def map(self, func):
        """Applies func to all commands in the sequence."""
        return map(func, self.commands)



class CommandPair(Sequence):
    """Represents a pair of commands, including the relationship between their paths.
    
    Private properties:
        commands (list): see parent class
        rel (enum): the relationship between the paths of the two commands.
            DIRECT_PARENT or DIRECT_CHILD or SEPARATE or SAME

    """
    # There is no need to consider any other relationship, e.g. when one path is a parent,
    # but not a direct parent of the other one, as unless there is a direct relationship,
    # the command on one path is not going to change the environment of the other.

    def __init__(self, command1, command2, rel):
        """Constructor.
        
        Args:
            command1 (Command): the first command in the pair
            command2 (Command): the second command in the pair
            rel (enum): the relationship between the paths of the two commands
        
        """
        self.commands = [command1, command2]
        self.rel = rel
        
    def info(self, debug=False):
        """Returns a human-readable string describing the object"""
        if self.rel == SEPARATE:
            return self.commands[0].info(debug) + " -x- " + self.commands[1].info(debug)
        if self.rel == DIRECT_CHILD:
            return self.commands[0].info(debug) + " -<> " + self.commands[1].info(debug)
        if self.rel == DIRECT_CHILD_ONLY:
            return self.commands[0].info(debug) + " ->> " + self.commands[1].info(debug)
        if self.rel == DIRECT_PARENT:
            return self.commands[0].info(debug) + " <>- " + self.commands[1].info(debug)
        if self.rel == DIRECT_PARENT_ONLY:
            return self.commands[0].info(debug) + " <<- " + self.commands[1].info(debug)
        if self.rel == SAME:
            return self.commands[0].info(debug) + " --- " + self.commands[1].info(debug)
        
    def clone(self):
        """Returns a shallow clone of the object. Commands are not mutable."""
        return self.__class__(self.commands[0], self.commands[1], self.rel)
        
    def getRelationship(self):
        return self.rel

    def getReverse(self):
        """Returns a new object with the pair reversed."""
        tmp = Sequence.getReverse(self)
        if self.rel == DIRECT_PARENT:
            tmp.rel = DIRECT_CHILD
        elif self.rel == DIRECT_PARENT_ONLY:
            tmp.rel = DIRECT_CHILD_ONLY
        elif self.rel == DIRECT_CHILD:
            tmp.rel = DIRECT_PARENT
        elif self.rel == DIRECT_CHILD_ONLY:
            tmp.rel = DIRECT_PARENT_ONLY
        return tmp
    
    def getLast(self):
        """Returns the last command in the pair."""
        return self.commands[1]


def CommandPairFactory():
    """Generates all possible command pairs. The file content values used will always be different."""
    for rel in [SEPARATE, DIRECT_CHILD, DIRECT_PARENT]:
        for c1 in CommandFactory(PATH1, 'New1'):
            for c2 in CommandFactory(PATH2, 'New2'):
                yield CommandPair(c1, c2, rel)
    for c1 in CommandFactory(PATH1, 'New1'):
        for c2 in CommandFactory(PATH1, 'New2'):
            yield CommandPair(c1, c2, SAME);

# We test command pairs and aim to answer the following questions:
# - Will the pair break all filesystems?
# - If not, can the pair be substituted by no commands at all?
# - If not, can the pair be substituted by a single command?
# - If not, can the pair be reversed?
# We are also interested in substitutions that extend the domain of the sequence.

for sq in CommandPairFactory():

    # We investigate filesystem models in which the two paths
    # have the relationship encoded in the command pair.
    # If the two commands in the pair use the same path,
    # we get the other path in the filesystem model out of the way
    # by using a SEPARATE relationship.
    fs_rel = sq.getRelationship()
    if fs_rel == SAME:
        fs_rel = SEPARATE
        
    # Does the pair break all filesystems?
    for fs in FilesystemFactory(fs_rel):
        fs.applySequence(sq)
        if not fs.isBroken():
            break # Skips "else" below
    else: # If none is broken
        print sq.info() + " \t== break"
        continue
        
    # Is the pair the same as no command at all?
    nothingEq = True  # Whether sq is equivalent to no commands on all filesystems
    nothingExt = True # Whether no commands is an extenstion of sq
    for fs in FilesystemFactory(fs_rel):
        fs_res = fs.clone()
        fs_res.applySequence(sq)
        if not fs_res.isSame(fs): nothingEq = False
        if not fs_res.isExtendedBy(fs): nothingExt = False
    if nothingEq:
        print sq.info() + " \t== (no commands)"
        continue
    if nothingExt:
        print sq.info() + " \t=[ (no commands)"
        continue
    

    # Try to find a single command with the same effect
    canSimplify = False
    for command in CommandFactory(sq.getLast().getPath(), sq.getLast().getEnd().getValue()):
        simplifiesEq = True  # Whether command is equivalent to sq on all filesystems
        simplifiesExt = True # Whether command extends sq
        for fs in FilesystemFactory(fs_rel):
            # Apply the original sequence
            fs_res = fs.clone()
            fs_res.applySequence(sq)
            # Apply the single command
            fs_single = fs.clone()
            fs_single.applyCommand(command)
            if not fs_res.isSame(fs_single): simplifiesEq = False
            if not fs_res.isExtendedBy(fs_single): simplifiesExt = False
        if simplifiesEq:
            canSimplify = True
            print sq.info() + " \t== " + command.info()
        elif simplifiesExt:
            canSimplify = True
            print sq.info() + " \t=[ " + command.info()
    
    if canSimplify: continue

    # Reverse sequence
    sq_rev = sq.getReverse()
    # print sq_rev.info()
    
    reverseEq = True  # Whether the reversed pair is equivalent to sq on all filesystems
    reverseExt = True # Whether the reversed pair extends sq
    for fs in FilesystemFactory(fs_rel):
        # Apply the original sequence
        fs_res = fs.clone()
        fs_res.applySequence(sq)
        # Apply the reverse sequence
        fs_rev_res = fs.clone()
        fs_rev_res.applySequence(sq_rev)
        if not fs_res.isSame(fs_rev_res): reverseEq = False
        if not fs_res.isExtendedBy(fs_rev_res): reverseExt = False
    if reverseEq:
        print sq.info() + " \t== " + sq_rev.info()
        continue
    if reverseExt:
        print sq.info() + " \t=[ " + sq_rev.info()
        continue
    
    print sq.info() + " \t(no rule)"

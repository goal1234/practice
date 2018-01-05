


# ---

input = [2, 1, [3, [4, 5], 6], 7, [8]]

def list_flatten(l, a = None):
    a = list(a) if isinstance(a, (list, tuple)) else []  # 首次置空之后a就是list
    for i in l:
        if isinstance(i, (list, tuple)):
            a = list_flatten(l, a)
        else:
            a.append(i)
    return a

list_flatten(input)

# ---

# 功能一, 功能二,功能一在前面

def missing_ranges(nums, lo, hi):
    res = []
    start = lo    # 绑定后再用
    for num in nums:
        if num < start:
            continue    # 在最小的地方开始,到第二循环
        if num == start: # 如果存在就加1,如果存在就跳过下一个循环
            start += 1
            continue    # 每次增加一个
        res.append(get_range(start, num-1))
        start = num + 1
    if start <= hi:   # 上线进行确认
        res.append(get_range(start, hi))
    return res

def get_range(n1, n2):
    if n1 == n2:
        return str(n1)
    else:
        return str(n1) + "->" + str(n2)

nums = [3, 5, 10, 11, 12, 15, 19]
print("original:", nums)
print("missing range: ", missing_ranges(nums,0,20))


# ---加最后一个
digits = [1, 2, 3, 4, 5, 6]
# digits+1 是错误的
def plusOne(digits):
    """
    :type digits: List[int]
    :rtype: List[int]
    """
    digits[-1] = digits[-1] + 1 # 最后一位加1
    res = []
    ten = 0
    i = len(digits)-1 # 最后一个就不循环了
    while i >= 0 or ten == 1:  # 考虑只有一个的长度
        sum = 0
        if i >= 0:
            sum += digits[i]
        if ten:
            sum += 1
        res.append(sum % 10)
        ten = sum / 10
        i -= 1
    return res[::-1]

# ---
def plus_one(digits):
    n = len(digits)
    for i in range(n-1, -1, -1):  #n-1是开始，-1是结束不包括所以是1，最后一个-1是步长向后退一步
        if digits[i] < 9:
            digits[i] += 1
            return digits
        digits[i] = 0
    digits.insert(0, 1)
    return digits

# ---
def plus_1(num_arr):
    for idx, digit in reversed(list(enumerate(num_arr))):
        num_arr[idx] = (num_arr[idx] + 1) % 10
        if num_arr[idx]:
            return num_arr
    return [1] + num_arr


# ---移动一个list，输入list同时输入位置就可以进行了--- #
#
# Rotate the entire array 'k' times
# T(n)- O(nk)
#
def rotate_one_by_one(nums, k):
    """
    :type nums: List[int]
    :type k: int
    :rtype: void Do not return anything, modify nums in-place instead.
    """
    n = len(nums)  # 取长度
    for i in range(k):  #移动很多次
        temp = nums[n-1]  # 取最后一位
        for j in range(n-1, 0, -1):
            nums[j] = nums[j-1]  #每一次向后挪移
        nums[0] = temp  # 加回去原来的


#
# Reverse segments of the array, followed by the entire array
# T(n)- O(n)
#
def rotate(nums, k):
    """
    :type nums: List[int]
    :type k: int
    :rtype: void Do not return anything, modify nums in-place instead.
    """
    n = len(nums)
    k = k % n
    reverse(nums, 0, n - k - 1)
    reverse(nums, n - k, n - 1)
    reverse(nums, 0, n - 1)


def reverse(array, a, b):
    while a < b:
        array[a], array[b] = array[b], array[a]
        a += 1
        b -= 1

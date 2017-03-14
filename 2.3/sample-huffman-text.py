text = """Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom"""

import collections
cnt = collections.Counter()
for line in  text.split('\n'):
    for word in line.strip().split(' '):
        cnt[word] += 1

for w, c in cnt.most_common():
    print w, c

print len(text)
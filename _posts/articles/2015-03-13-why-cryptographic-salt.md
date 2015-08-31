---
layout: post
title: "What is a salt and Why Is It Useful?"
excerpt: "No more password managers until you read this!"
categories: articles
tags: [salt,cryptography,cryptographic,security,computers]
comments: true
share: true
ads: true
redirect_from: ['/post/113483796244/why-cryptographic-salt/', '/post/113483796244/']
---

<p>Someone recently asked me why we should use a salt if were simply going to store it in plaintext. I mean, after all, if an attacker is trying to crack a password, they simply add the salt to whatever generated string theyre checking and it has seemingly negligible effect. While this is true, it misses some fundamental concepts as to what a cryptographic salt actually adds by way of security.</p><h2>Wait a second. Salt? Thats what I put on my fries.</h2><p>Well, yes. Salt is the delicious white crystalline substance which we often use to add flavor to our food. However, it can also be used to add flavor (so to speak) to our passwords. When people are talking about salting in the context of cryptography, they are typically referring to a <i>random string</i> which is deterministically combined (i.e. usually appended or prepended) with some value which is then <a href="http://en.wikipedia.org/wiki/Hash_function" target="_blank">hashed</a>. For instance, if we had some hash function <i>h(x)</i>, we might salt a password as follows:</p>

```javascript
// This is Berstein's hash, btw
var h = function(str) { return str.split('').reduce(function(x, y) { return 33 * x + y.charCodeAt(); }, 0); };
var SALT_LEN = 10;
var salt = [];

// Random salt
for(var i = 0 ; i < SALT_LEN ; ++i) {
  // Limit to printables
  var value = 32 + Math.ceil(Math.random() * 94);
  salt.push(value);
}

// Convert back to string
salt = salt.reduce(function(x,y){ return x + String.fromCharCode(y); }, "");

var password = "myPassword";

console.log("Unsalted hash: " + h(password));
console.log("Salting password: " + password + " with " + salt);
console.log("Result: " + h(salt + password));
```

<p>As you can see, all we are doing is simply combining (i.e. prepending) our salt with our secret (i.e. our password) before we hash it. Pretty simple, huh?</p><h2>What does this even buy us?</h2><p>Naturally, if an attacker is trying to crack a single password, this buys us nothing. In fact, a brute-force attack on this hash would be as effective as a brute-force attack on any hash (i.e. exponentially slow). However, there is another class of attacks which this guards against. In particular, this thwarts the attempts of <a href="http://en.wikipedia.org/wiki/Rainbow_table" target="_blank">rainbow table</a> attacks. Similarly, by salting each password with a unique, this makes it even harder for an attacker to actually execute such an attack on a mass scale.</p><p><b>Unsalted hashes. </b>So how exactly does this attack work? Well, simply put, many attackers try to brute force many hashes and they save the results. Consequently, they come up with a list of values that map to a hash for a given function.</p><blockquote><div>NOTE: Remember, when youre attacking a hash a <a href="http://en.wikipedia.org/wiki/Collision_%28computer_science%29" target="_blank">collision</a> is just as good as the real input.</div></blockquote><p>This table is precomputed and simply used as a lookup. For sake of example, say we have computed a rainbow table for the hash function <i>h</i> shown above. Typically, we would probably store the values in a <a href="http://en.wikipedia.org/wiki/Hash_table" target="_blank">hash table</a> for quick lookup. Now, say we are trying to crack the passwords of a user from a database dump we happened to acquire (read: if you actually acquire one of these, youre probably breaking the law) and we notice that the users hash value is 4282592. Since we have computed our rainbow table and stored it as a hash table, we will do a simple lookup:</p>

```javascript
var plaintext = rainbowTable[4282592];
// The result here for an unsalted hash would be "test" or a collision
```

<p>We have now successfully recovered a plaintext which we can use to login as this user on the service (again, regardless of whether it is the <i>real</i> password or not).</p><p><b>Hashes with same salt. </b>With a salted hash, however, we would have made this attack harder to achieve. Instead of using the original precomputed table, the attacker would have to precompute a <i>new</i> table with the salt combined appropriately. Remember, rainbow attacks work well because they are computed once (often by many people) and then reused many times. It is <i>very expensive</i> to compute a rainbow table. However, once a rainbow table is computed for your particular salted implementation (since you used the same salt), cracking these hashes is as trivial as with the unsalted hash. That is, if you salt your table with some fixed <i>s</i>, then computing a table <i>h</i>(<i>s </i>+<i> x</i>) for all outputs of <i>h</i> is feasible.</p><p><b>Hashes with random salt.</b> This technique tends to work better in most cases. In this case, there is no way to compute a rainbow table for your hashing scheme. Even though the attacker knows the salt, this is effectively reduced to a <i>brute-force</i> attack. Namely, since <i>every salt is different</i>, there is no way to effectively compute a rainbow table for all of your stored hashes. What I mean is, since <i>s</i> is not fixed in this case, <i>h</i>(<i>s</i> + <i>x</i>) cannot be effectively computed for all outputs of <i>h</i> since the combination of random <i>s</i> and <i>x</i> will have an unpredictable effect on the hash function. Thus, for a <i>specific</i> value <i>s</i> (i.e. one random hash) for a particular <i>x</i> youre trying to crack, you would need to (theoretically) explore the entire output space of <i>h</i>(<i>s</i> + <i>x</i>) before uncovering an acceptable output value for x. This is brute-force.</p><h2>How do I do this in practice?</h2><p>Fortunately, you are not likely going to have to implement salting schemes yourself anymore. That is, of course, if you decide to use a hashing algorithm such as <a href="http://en.wikipedia.org/wiki/Bcrypt" target="_blank">bcrypt</a>. Bcrypt adds your salt directly as part of the hash and&ndash; if youre using a library&ndash; often abstracts away the messy details of generating a proper salt for your hash. That said, you should see <a href="http://blog.ircmaxell.com/2012/12/seven-ways-to-screw-up-bcrypt.html" target="_blank">this post</a> for more information on how you can easily screw up using bcrypt.</p><p>Next time someone starts talking about hashing and salting, you now have at least an idea about what theyre talking about. Whats more, you might be able to do a little more research and even help ensure the proposed scheme is actually effective against certain attack classes!</p>

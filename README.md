# abacus

**Collect and classify personal cash flows in a database contained in one file.**

1. SQLite database for storing transactions, and functions for accessing it
2. Machine Learning functions for predicting new transactions based on old ones
3. Web-based application interface for ease of use




## 1. SQLite Database

The database schema is shown below.
I wrote some functions for easily accessing the database.
Usually their name says what they do: `Select`, `Insert`, `Update`.
`Create_testDB` creates a test database with 100 accounts, 2 personalAccounts and 1000 transactions.

![database schema]("./vignette/DBschema.png")

### Accounts

Holds accounts involved in every transaction and personal accounts.
Their IDs are used in the other tables as shown.
*personalAccounts* only holds you own accounts.
I made an extra table for that because personal accounts are used more frequently and need one more column.
Since *accounts* will get very big I thoughts its worth it creating an extra table.

### Transactions

Actual transactions with all the information used by your bank.
Column *type* is a label, which will be used by the classifier (see 2. Machine Learning).
In principle this could already be used to create cashflows.
However, I thought 1. it might be useful to aggregate the labels further and 2. that cash withdrawals need to be handled seperately.
Say cash withdrawals are always labelled as *cash withdrawal*.
Sometimes it might be useful to separate a cash withdrawal into different labels to be more precise.
Therefore there is another table *cashflow*.

### Cashflow

The *cashflow* table is mainly derived from *transactions*.
Transactions are mainly aggregated into time intervalls.
It appears a little overkill to have an extra table for that, but I thought the web-app will use this information very often 
and it would be unnecessary to always compute it from the main *transactions* table.
One might also want to aggregat a few labels.
Therefore, the corresponding column is called *category* in this table.
Also, one might want to split labels from transactions into more categories (e.g. a cash withdrawal).
If this happens another column (*comment*) can store the original label.

### Capital

This is for holding actual balances at certain time points.
This should not be needed if all transactions are thoroughly entered.
However, once cash is involved this never happens.
So, this can be used as a sanity check.

### Storage

This table hold objects which are used by the *abacus* package.
These objects can change and are specific to the database.
Therefore, they have their own table.
The objects are serialized for saving.
They can be accessed with the `SelectBLOB`, `InserBLOB`, `UpdateBLOB` functions.




## Tests

 run `test_dir(system.file("tests", "", package = "abacus"))` to test package functions

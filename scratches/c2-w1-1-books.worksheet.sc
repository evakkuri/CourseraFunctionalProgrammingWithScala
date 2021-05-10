case class Book(title: String, authors: List[String])

// Example list of books
val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
        authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
        authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
        authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
        authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
        authors = List("Odersky, Martin")),
    Book(title = "Testbook 1",
        authors = List("Bloch, Joshua")),
    Book(title = "Testbook 2",
        authors = List("Bloch, Joshua"))
)

// All books written by "Bloch"
for (b <- books; a <- b.authors if a startsWith "Bloch")
yield b.title

// All books that include word "Program"
for (b <- books if b.title.indexOf("Program") >= 0)
yield b.title

// All authors with at least 2 books written. Note: if there are 3 books or
// more, you will get (n - 1) * (n - 2) entries
for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
} yield a1

// We could use distinct
{
    for {
        b1 <- books
        b2 <- books
        if b1.title < b2.title
        a1 <- b1.authors
        a2 <- b2.authors
        if a1 == a2
    } yield a1
}.distinct

// We could also make books into a Set

// For expressions are translated by the compiler to flatMap, withFilter and map
// operations. This means that any custom class that implements these methods
// can be used in for expressions.
books flatMap (b =>
   b.authors withFilter(a => a startsWith "Bird" )
   map(y => b.title))

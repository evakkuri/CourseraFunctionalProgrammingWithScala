def repeat(command: => Unit)(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else repeat(command)(condition)
}



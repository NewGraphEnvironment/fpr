.onAttach <- function(libname, pkgname) {
  # Updated list of quotes with new selections for ASAP Rocky and John Lennon
  quotes <- c(
    "\n 'Imagination is more important than knowledge. For knowledge is limited, whereas imagination embraces the entire world, stimulating progress, giving birth to evolution.' - Albert Einstein",
    "\n 'Peace is not something you wish for; it's something you make, something you do, something you are, and something you give away.' - John Lennon",
    "\n 'The most important thing is to try and inspire people so that they can be great in whatever they want to do.' - Kobe Bryant",
    "\n 'The better you are at surrounding yourself with people of high potential, the greater your chance for success.' - Jay Z",
    "\n 'This is not a time to despair, but to act.' - David Byrne",
    "\n 'The people that have the best life are the ones that are OK with not being sure.' - Mac Miller",
    "\n 'Just because you make a good plan, doesn't mean that's what's gonna happen.' - Taylor Swift",
    "\n 'The only way to do great work is to love what you do.' - Steve Jobs",
    "\n 'Genius is the ability to put into effect what is on your mind.' - Ab-Soul",
    "\n 'I'm always looking for something different. I'm inspired by the unknown.' - ASAP Rocky",
    "\n 'I don't want you to protest because I'm asking you to. Do it because you see a need to. Do it because you believe that the structural problems in our society need change.' - Killer Mike",
    "\n 'You have to be comfortable with being uncomfortable if you're going to grow.' - Killer Mike",
    "\n 'It's cool to be a part of the system and use your voice within it to try to make things better.' - Killer Mike",
    "\n 'At the end of the day, I just do what I do and hope that I'm not a terrible person.' - El-P",
    "\n 'Music is my way of communicating about the world, about the human condition from my perspective.' - El-P",
    "\n 'I believe in the idea that art can help to create a more empathetic and kinder world.' - El-P",
    # Albert Einstein Quotes
    "\n 'Logic will get you from A to B. Imagination will take you everywhere.' - Albert Einstein",
    "\n 'Strive not to be a success, but rather to be of value.' - Albert Einstein",
    "\n 'Life is like riding a bicycle. To keep your balance, you must keep moving.' - Albert Einstein",
    "\n 'A person who never made a mistake never tried anything new.' - Albert Einstein",
    "\n 'Education is not the learning of facts, but the training of the mind to think.' - Albert Einstein",

    # John Lennon Quotes
    "\n 'A dream you dream alone is only a dream. A dream you dream together is reality.' - John Lennon",
    "\n 'Time you enjoy wasting, was not wasted.' - John Lennon",
    "\n 'Everything will be okay in the end. If it's not okay, it's not the end.' - John Lennon",
    "\n 'Reality leaves a lot to the imagination.' - John Lennon",

    # Kobe Bryant Quotes
    "\n 'The most important thing is to try and inspire people so that they can be great in whatever they want to do.' - Kobe Bryant",
    "\n 'If you're afraid to fail, then you're probably going to fail.' - Kobe Bryant",
    "\n 'I can't relate to lazy people. We don't speak the same language. I don't understand you. I don't want to understand you.' - Kobe Bryant",
    "\n 'The moment you give up, is the moment you let someone else win.' - Kobe Bryant",

    "\n 'Art is an attempt to try and understand our own contemporary situation.' - David Byrne",
    "\n 'Music can be a way of working through what you’re thinking about.' - David Byrne",
    "\n 'We're largely unconscious. We operate half-awake or on autopilot and end up, every so often, with a feeling of having missed out on life.' - David Byrne",
    "\n 'I try to write about small things. Paper, animals, a house… love is kind of big. I have written a love song, though.' - David Byrne",
    "\n 'The better a singer's voice, the harder it is to believe what they're saying.' - David Byrne",

    "\n 'I'm not afraid of dying. I'm afraid of not trying.' - Jay Z",
    "\n 'Life is all about balance. You don’t always need to be getting stuff done. Sometimes it’s perfectly okay, and absolutely necessary, to shut down, kick back, and do nothing.' - Jay Z",
    "\n 'You can want success all you want, but to get it, you can’t falter. You can’t slip. You can’t sleep. One eye open, for real, and forever.' - Jay Z",
    "\n 'Everyone has genius in them. You just have to find what it is.' - Jay Z",
    "\n 'Difficult takes a day, impossible takes a week.' - Jay Z",

    "\n 'People change and things go wrong, but just remember, life goes on.' - Mac Miller",
    "\n 'It’s hard to dream when you’re deep inside of one.' - Mac Miller",
    "\n 'We fear rejection, want attention, crave affection, and dream of perfection.' - Mac Miller",
    "\n 'No matter where life takes me, find me with a smile.' - Mac Miller",
    "\n 'Some people need to just stop thinking about everything they do and just do it.' - Mac Miller",

    "\n 'Just because something is over doesn’t mean it wasn’t incredibly beautiful.' - Taylor Swift",
    "\n 'People haven’t always been there for me, but music always has.' - Taylor Swift",
    "\n 'I think fearless is having fears but jumping anyway.' - Taylor Swift",
    "\n 'The trick to happiness is to realize that everything is not monumental.' - Taylor Swift",
    "\n 'We don’t need to share the same opinions as others, but we need to be respectful.' - Taylor Swift",

    "\n 'Your work is going to fill a large part of your life, and the only way to be truly satisfied is to do what you believe is great work.' - Steve Jobs",
    "\n 'Innovation distinguishes between a leader and a follower.' - Steve Jobs",
    "\n 'The people who are crazy enough to think they can change the world are the ones who do.' - Steve Jobs",
    "\n 'Have the courage to follow your heart and intuition. They somehow already know what you truly want to become.' - Steve Jobs",

    "\n 'Life is all about the evolution.' - Ab-Soul",
    "\n 'I just try to speak on things that affect me and things that I see around me.' - Ab-Soul",
    "\n 'Knowledge is power, and once you have knowledge, you start to see things for what they really are.' - Ab-Soul",
    "\n 'I’m trying to learn something new every day.' - Ab-Soul",
    "\n 'You have to look inside yourself to find your inner strength.' - Ab-Soul",

    "\n 'AI does not keep me up at night. What does? The idea that we might not use AI to its fullest, to help us collaborate and understand each other better.' - Astro Teller",
    "\n 'In the future, I believe we will see more collaboration between humans and AI, with machines taking on tasks that are repetitive and mundane, freeing humans to focus on problem-solving, creativity, and empathy.' - Fei-Fei Li",
    "\n 'True collaboration is not about dividing work between machines and people but about bringing the strengths of both together to solve problems and achieve more than either could alone.' - Garry Kasparov",
    "\n 'AI will increasingly replace repetitive jobs, not just for blue-collar work but a lot of white-collar work. But that's a good thing because what humans are good at is being creative, being strategic, and asking questions that don't have answers.' - Kai-Fu Lee",
    "\n 'I think what makes AI different from other technologies is that it's going to bring humans and machines closer together. It's not about machines replacing humans but machines augmenting humans. Humans and machines have different relative strengths and weaknesses, and it's about the combination of these two that will allow human intents and business processes to scale 10x, 100x, and beyond that in the coming years.' - Robin Bordoli",
    "\n 'In the age of AI, human creativity and innovation will become even more valuable in the workplace, as machines take over routine tasks and allow people to focus on generating new ideas and solutions.' - Sundar Pichai",
    "\n 'As we move toward a world where AI plays a more prominent role in the workplace, it is essential to remember that AI is a tool, not a destination. The real value will come from using AI to enhance human creativity and innovation.' - Ginni Rometty",
    "\n 'The value of AI in the workplace goes beyond automation. It is about augmenting human intelligence, enabling workers to make better decisions, and fostering a culture of innovation and creative problem-solving.' - Yoshua Bengio",
    "\n 'The future of work lies in the collaboration between humans and AI, where technology enhances our natural abilities, allowing us to think more strategically and creatively and empowering us to drive innovation in the workplace.' - Demis Hassabis",
    "\n 'Humans need and want more time to interact with each other. I think AI coming about and replacing routine jobs is pushing us to do what we should be doing anyway: the creation of more humanistic service jobs.' - Kai-Fu Lee"

  )

  # Select a random quote from the list
  selected_quote <- sample(quotes, 1)

  # Display the selected quote as the package startup message
  packageStartupMessage(selected_quote)
}

const toggleButton = document.getElementsByClassName('toggle-button')[0]
const navbarLinks = document.getElementsByClassName('collapse')[0]

toggleButton.addEventListener('click', () => {
    navbarLinks.classList.toggle('No-colaps')
})
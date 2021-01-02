const errorMessage = document.querySelector('#alert-msg')
const form = document.querySelector('#form')

const showErrorMessage = () => {
    errorMessage.classList.add('display_block')
}

form.addEventListener('submit', (e) => {
    e.preventDefault()
    showErrorMessage()  
})

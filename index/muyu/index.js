const muyu = document.getElementById("muyu");
const counter = document.getElementById("count");

const audio = new Audio('./muyu.mp3')

let count = 0;

muyu.addEventListener("click", (ev) => {
    muyu.className = 'big';
    shrink();

    count++;
    counter.innerText = '功德：' + count;

    audio.currentTime = 0;
    audio.play()
})

let handle;
function shrink() {
    if (handle) clearTimeout(handle);
    handle = setTimeout(() => muyu.className = 'small', 80);
}
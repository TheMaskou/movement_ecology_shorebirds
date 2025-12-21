// Fade hero background on scroll
window.addEventListener('scroll', function() {
  const scrollPosition = window.scrollY;
  const hero = document.querySelector('.hero-section');
  
  if (hero) {
    // Calculate opacity based on scroll position
    const fadeStart = 100;
    const fadeEnd = 600;
    
    if (scrollPosition <= fadeStart) {
      hero.style.opacity = 1;
    } else if (scrollPosition >= fadeEnd) {
      hero.style.opacity = 0;
    } else {
      const opacity = 1 - (scrollPosition - fadeStart) / (fadeEnd - fadeStart);
      hero.style.opacity = opacity;
    }
  }
});
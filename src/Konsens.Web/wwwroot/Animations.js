(function(window) {

    var isWide = false;

    function animate(startup) {
        if (window.innerWidth >= 769) {
            if (!startup && !isWide)
                return;

            isWide = false;
            anime({
                targets: '.home-title',
                translateX: '-50%',
                translateY: 0,
                delay: startup ? 1000 : 0
            });
            anime({
                targets: '.home-page__content',
                translateX: '50%',
                translateY: 0,
                opacity: '1',
                delay: startup ? 1000 : 0
            });
        } else {
            if (!startup && isWide)
                return;

            isWide = true;
            anime({
                targets: '.home-title',
                translateX: 0,
                translateY: '-50%',
                delay: startup ? 1000 : 0
            });
            anime({
                targets: '.home-page__content',
                translateX: 0,
                translateY: '50%',
                opacity: '1',
                delay: startup ? 1000 : 0
            });
        }
    }

    window.configureAnimations = () => {
        window.addEventListener('resize', () => animate());
        animate(true);
    };

})(window);

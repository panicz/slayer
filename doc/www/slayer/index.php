<?php

function template($file, $vars = array()) {
    $matches = array();
    $content = file_get_contents($file);
    preg_match_all('/\$\w+\b/', $content, $matches);
    $matches = array_filter($matches);
    $matches = call_user_func_array('array_merge', $matches);
    foreach($matches as $_name) {
        $name = substr($_name, 1); // skip the initial dollar
        if(isset($vars[$name])) {
            $content = preg_replace('/\\'.$_name.'/', $vars[$name], $content);
        }
    }
    return $content;
}

function content_file_name($name) { return "content/" .  $name . ".html"; }

function load_content($filename) {
    $file = file($filename);
    $title = array_shift($file);
    $name = array_shift($file);
    $content = implode("\n", $file);
    return array(
        'title' => $title,
        'name' => $name,
        'content' => $content
    );
}

$page = isset($_GET['page']) ? $_GET['page'] : 'intro';

$content_file = content_file_name($page);

if(!file_exists($content_file)) {
    $content_file = content_file_name('intro');
}

print template('template.html', load_content($content_file));

?>

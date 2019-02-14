function identity(x){
  return x;
}

function hd(arr){
  return arr[0];
}

function hdTest(){
  var arr = [1,2,3];
  return hd(arr) == 1;
}

function tl(arr){
  return arr.slice(1);
}

function tlTest(){
  var arr = [1,2,3];
  var tail = tl(arr);
  return arr[0] == 1 &&
         arr[1] == 2 &&
         arr[2] == 3 &&
         arr.length == 3 &&
         tail[0] == 2 &&
         tail[1] == 3 &&
         tail.length == 2;
}

function equal(arr1, arr2){
  if(!Array.isArray(arr1) ||
     !Array.isArray(arr2)){
    return false;
  }
  if(arr1.length == 0 ||
     arr2.length == 0){
    return arr1.length == 0 &&
           arr2.length == 0;
  }
  if(Array.isArray(arr1[0])){
    return equal(arr1[0], arr2[0]) &&
           equal(tl(arr1), tl(arr2));
  }
  return arr1[0] == arr2[0] &&
         equal(tl(arr1), tl(arr2));
}

function equalTest(){
  return equal([1,2,3], [1,2,3]) &&
         equal([[1,2,3], [4,5,6]], [[1,2,3], [4,5,6]]) &&
         equal([[1,2,3], 4], [[1,2,3], 4]) &&
         !equal([[1,2,4], 4], [[1,2,3], 4]) &&
         !equal([[1,2,3], 3], [[1,2,3], 4]) &&
         !equal([1,2,3], [1,2,4]) &&
         !equal([1,2], [1,2,3]) &&
         !equal([1,2,3], [1,2]) &&
         !equal([[1,2,3]], [1,2,3]) &&
         equal([], []);
}

function cons(arr, elem){
  copy = arr.slice(0);
  copy.unshift(elem);
  return copy;
}

function consTest(){
  var arr = [1,2,3];
  var arr2 = cons(arr, 0);
  return arr.length == 3 &&
         arr[0] == 1 &&
         arr[1] == 2 &&
         arr[2] == 3 &&
         arr2.length == 4 &&
         arr2[0] == 0 &&
         arr2[1] == 1 &&
         arr2[2] == 2 &&
         arr2[3] == 3;
}

function reverse(arr){
  copy = arr.slice(0);
  copy.reverse();
  return copy;
}

function reverseTest(){
  var arr = [1,2,3];
  var rev = reverse(arr);
  return equal([1,2,3], arr) &&
         equal([3,2,1], rev);
}

//function concat(arr1, arr2){
   //TODO: make sure this returns a new array instead of just modifying arr1
  //return arr1.concat(arr2);
//}

function map(fun, arr, maybe_results){
  var results = (maybe_results == undefined ? [] : maybe_results.slice(0));
  var next;
  if(arr.length == 0){
    return reverse(results);
  }else{
    return map(fun, tl(arr), cons(results, fun(hd(arr))));
  }
}

function mapTest(){
  var result = map(function(x){ return x + 1 }, seq(3));
  return equal([2,3,4], result);
}

function seq(maybe_next, maybe_end, maybe_sequence){
  var next;
  var end;
  var sequence;
  if(maybe_end == undefined){
    end = maybe_next;
    next = 1;
  }else{
    next = maybe_next;
    end = maybe_end;
  }
  if(maybe_sequence == undefined){
    sequence = [];
  }else{
    sequence = maybe_sequence.slice(0);
  }
  if(next > end){
    return sequence;
  }else{
    sequence.push(next);
    return seq(next + 1, end, sequence);
  }
}

function seqTest(){
  var seq1 = seq(3);
  var seq2 = seq(4,6);
  return equal([1,2,3], seq1) &&
         equal([4,5,6], seq2);
}

function seqBy(start, end, step, maybe_sequence){
  var sequence;
  if(maybe_sequence == undefined){
    sequence = [];
  }else{
    sequence = maybe_sequence;
  }
  if(start > end){
    return reverse(sequence);
  }
  var next = parseFloat((start + step).toFixed(6));
  return seqBy(next, end, step, cons(sequence, start));
}

function seqByTest(){
  var seq = seqBy(3, 3.3, 0.1);
  return equal([3, 3.1, 3.2, 3.3], seq);
}

function repeat(elem, count){
  var sequence = seq(count);
  return map(function(x){ return elem }, sequence);
}

function repeatTest(){
  var arr = repeat('x', 4);
  return equal(['x', 'x', 'x', 'x'], arr);
}

function filter(fun, arr){
  return filter_(fun, arr, []);
}

function filter_(fun, arr, results){
  if(arr.length == 0){
    return reverse(results);
  }

  if(fun(hd(arr))){
    return filter_(fun, tl(arr), cons(results, hd(arr)));
  } else {
    return filter_(fun, tl(arr), results.slice(0));
  }
}

function filterTest(){
  var even = function(x){return x % 2 == 0};
  var results = filter(even, [1,2,3,4]);
  return equal([2,4], results);
}

function foldl(fun, arr, acc){
  if(arr.length == 0){
    return acc;
  }
  return foldl(fun, tl(arr), fun(hd(arr), acc));
}

function zip(arr1, arr2){
  return zip_(arr1, arr2, []);
}

function zip_(arr1, arr2, zips){
  if(arr1.length == 0 ||
     arr2.length == 0){
    return reverse(zips);
  }

  return zip_(tl(arr1),
              tl(arr2),
              cons(zips, [hd(arr1), hd(arr2)]));

}

function zipTest(){
  var zipped = zip([1,2,3], [4,5,6]);
  return equal([[1,4],[2,5],[3,6]], zipped);
}

function zip3(arr1, arr2, arr3){
  return zip3_(arr1, arr2, arr3, []);
}

function zip3_(arr1, arr2, arr3, zips){
  if(arr1.length == 0 ||
     arr2.length == 0 ||
     arr3.length == 0){
    return reverse(zips);
  }

  return zip3_(tl(arr1),
              tl(arr2),
              tl(arr3),
              cons(zips, [hd(arr1), hd(arr2), hd(arr3)]));
}

function zip3Test(){
  var arr1 = [1,2,3];
  var arr2 = [4,5,6];
  var arr3 = [7,8,9];

  var zipped = zip3(arr1, arr2, arr3);
  return equal([[1,4,7],[2,5,8],[3,6,9]], zipped);
}

function pairs(arr){
  return pairs_(arr);
}

function pairs_(arr, maybe_pairs){
  var pairs;
  if(maybe_pairs == undefined){
    pairs = [];
  }else{
    pairs = maybe_pairs;
  }

  if(arr.length < 2){
    return reverse(pairs);
  }

  return pairs_(tl(arr), cons(pairs, [hd(arr), hd(tl(arr))]));
}

function pairsTest(){
  var arr = [1,2,3,4,5];
  pairs1 = pairs([1,2,3,4,5]);
  pairs2 = pairs([1,2]);
  return equal([[1,2],[2,3],[3,4],[4,5]], pairs1) &&
         equal([[1,2]], pairs2) &&
         pairs([]).length == 0 &&
         pairs([1]).length == 0;
}

function rotate(arr){
  if(arr.length == 0){
    return [];
  }else if(arr.length == 1){
    return arr.slice(0);
  }
  return reverse(cons(reverse(tl(arr)), hd(arr)));
}

function rotateTest(){
  var result = rotate([1,2,3]);
  return equal([2,3,1], result) &&
         rotate([]).length == 0 &&
         rotate([1])[0] == 1 &&
         rotate([1]).length == 1;
}

function all(fun, arr){
  if(arr.length == 0){
    return true;
  }
  return fun(hd(arr)) && all(fun, tl(arr));
}

function allTest(){
  var fun = function(x){ return x == 1 };
  return all(fun, [1, 1, 1]) &&
         !all(fun, [1, 1, 2]) &&
         all(fun, []);
}

/*
 * wrap a function that takes two arguments
 * in a fuction that takes an array and calls
 * that function with the first 2 elements of the
 * array as the two arguments
 */
function apply2(f){
  return function(p){return f(p[0], p[1])};
}

function apply2Test(){
  var f = function(x, y){ return x == y };
  return (apply2(f))([1, 1]);
}

function concat(arr1, arr2){
  return concat_(reverse(arr1), arr2);
}

function concat_(arr1, arr2){
  if(arr1.length == 0){
    return arr2;
  }
  return concat_(tl(arr1), cons(arr2, hd(arr1)));
}

function concatTest(){
  return equal([1,2,3,4,5,6], concat([1,2,3],[4,5,6])) &&
         equal([1,2], concat([], [1,2])) &&
         equal([1,2], concat([1,2], [])) &&
         equal([], concat([], []));
}

function contains(elem, arr){
  return filter(function(x){return x === elem}, arr).length > 0;
}

function containsTest(){
  return contains(1, [1,2,3]) &&
         !contains(4, [1,2,3]) &&
         contains("a", ["a", "b", "c"]) &&
         !contains("d", ["a", "b", "c"]) &&
         !contains(1, ["1", 2, 3]);
}

function unique(arr){
  var fun = function(x, acc){
              if(contains(x, acc)){
                return acc;
              }else{
                return cons(acc, x);
              }};
  return reverse(foldl(fun, arr, []));
}

function uniqueTest(){
  return equal([1,2,3], unique([1,1,1,2,3,3])) &&
         equal([1,2,3], unique([1,2,3,1,3])) &&
         equal(["a", "b", "c"], unique(["a", "b", "c", "a", "b"]));
}

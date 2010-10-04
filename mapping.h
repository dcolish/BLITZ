#ifndef MAPPING_H
#define MAPPING_H

template <class Key, class Value> class Mapping;
template <class Key, class Value> class Bucket;


//----------  Mapping Class Stuff  ----------

template <class Key, class Value> class Mapping {
  public:
    void    enter (Key * k, Value * v);
    Value * find (Key * k);                       // Search all scopes
    Value * findInTopScope (Key * k);
    int     alreadyDefined (Key * k);             // Search top scope only
    void    print (int indent);
    Value * getFirst ();                          // Used to iterate through
    Value * getNext ();                           //     all values in top scope
    Key *   getItsKey ();                         //     Return key of last value
    void    printOffsetToSelector (const char * title); // Used for "offsetToSelector" maps
    void    printSelectorToOffset ();             // Used for "selectorToOffset" maps
    Mapping (int initSize, Mapping * superMap);   // Zero is OK; NULL is OK
    ~Mapping ();

  private:
    Mapping * superMap;
    int numberOfElements;
    int sizeOfArray;
    Bucket < Key, Value> * * array;
//    int iteratorLastIndex;
    Bucket < Key, Value> * iteratorLastBucket;
    Bucket < Key, Value> * firstInsertedBucket;
    Bucket < Key, Value> * lastInsertedBucket;

    int nextLargerSize (int oldSize);
    void rehashIfNecessary ();
    int arrayIndex (Key *);
    void printKeyValue (Key * key, Value * value, int indent);
};

template <class Key, class Value> class Bucket {
  public:
    Bucket * next;
    Bucket * nextForIterator;
    Key * key;
    Value * value;
    Bucket () {
      next = NULL;
      nextForIterator = NULL;
      key = NULL;
      value = NULL;
    }
    ~ Bucket () { }
};

void testMapping ();

#endif

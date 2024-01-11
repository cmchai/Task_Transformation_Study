var consentText = ['<div class = "InstrBx"><h1>Welcome to the experiment</h1>'+
                   '<p>This is a research project conducted at Ghent University. The data we collect during the experiment are not linked to any potentially identifying information, \
                  and will be used solely for research purposes. Anonymized data from the study will be registered and archived at a trusted public data repository, in line with current \
                  data sharing practices. You are free to stop the experiment by closing your browser window at any time, which will not be of any disadvantage to you. By clicking "I agree", \
                  you affirm that you are at least 18 years of age, and understand the nature of your participation in this research. If you do not wish to participate, please close this window.<p></div>'];
var mainInstruction_part1 = [
                             //////////////////////////////////// the first page //////////////////////////////////////////
                             '<div class = "InstrBx"><h1>Welcome to the experiment!</h1>' +
                             '<p>Please read the instruction carefully since this is not an easy task</p>' +
                             '<p>Today you will be performing a task that requires categorizing different images. During this task, you will see three images\
                              presented simultaneously, each depicting an <b>animal</b>, a <b>place</b>, or a <b>vehicle</b> respectively, as shown below:</p>' +
                            
                             '<p><img src="instruction_images.jpg" class = "imgInst"></img></p></div>',
                             //////////////////////////////////// the second page //////////////////////////////////////////
                             '<p><div class = "InstrBx">Before showing these images, a cue will be presented informing you which image you should focus on (animal, place, or vehicle), as well as which property you should be categorizing\
                              them on: age, size, or location. You need this cue to prepare for the task. For example, you may see the following instruction:</p></div>' +

                             '<p><img src="instruction_cue.jpg" style="width:320 height:280"></img></p>',
                             //////////////////////////////////// the third page //////////////////////////////////////////
                             '<p><div class = "InstrBx">In this example, you need to evaluate if the animal in the picture is either a young or old animal. As you could see in the previous picture, the animal shown in the upper image was a young\
                              kitten. Therefore, you should respond by pressing the key corresponding to the \"young\" category, which, in this case, is the <b>left</b> key. Always use \"F\" or \"J\" to indicate your left or right response. respectively.\
                              For example, if the cue shows \"young | old\", which \"young\" is on the <b>left</b> side of the \"|\" and thus requires a left response by pressing the \"F\" key. However, if the cue shows \"old | young\" instead, you need \
                              to press the right key (\"J\") to indicate it is a \"young\" image. <b>Please always try to respond as fast and accurate as possible!</b></p>' +

                              '<p>Between the cue and the image, there will be three fixation crosses (\"+\") indicating the location of the upcoming images. <b>These crosses will increase in size with time elapsing</b>.</p>' +

                              '<p>To see an example trial, please press NEXT</p></div>'
                             ];
var mainInstruction_part2 = [
                             //////////////////////////////////// the first page //////////////////////////////////////////
                             '<p><div class = "InstrBx">The task you just performed was a <b>\"regular\"</b> trial. In addition to regular trials, there will also be <b>\"transform\"</b> trials, where the task will change before the images appear on\
                              screen. Specifically, in this case, a new task cue will appear on the screen right before the images\' onset, and you will need to perform the new task, and forget about the originally cued task.</p>' + 

                              '<p>To see an example trial, please press NEXT.</p></div>'
                             ];

var mainInstruction_part3 = [
                             //////////////////////////////////// the first page //////////////////////////////////////////
                             '<p><div class = "InstrBx">Now, you experienced both regular and transform trials. The transform trials are more difficult than regular trials because they require you to quickly switch tasks. \
                              To better prepare for them, you can actually rely on the time interval between the original task cue and the images. <b>Specifically, the longer you have to wait following the first cue, the more likely the task will transform.</b></p>' +

                              '<p>In the experiment, there are six blocks. Three are blocks with <b>only</b> regular trials. The other three blocks will contain <b>both</b> regular and transform trials. \
                              In these blocks, <b>you should try to be well-prepared for transform trials</b> because they can appear anytime in the block.</p></div>',
                             //////////////////////////////////// the second page //////////////////////////////////////////
                              '<p><div class = "InstrBx">Importantly, in blocks that contain both regular and transform trials, please try to perform as good as possible on BOTH regular and transform trials. <b>The participation credit will be awarded to you as long as you perform well on BOTH trial types.</b></p></div>', 
                             //////////////////////////////////// the third page //////////////////////////////////////////
                              '<p><div class = "InstrBx">Before doing the actual experiment, we will present each image to you and ask you to categorize them, so you can become familiar with each image and their categories.</p>'+
                              '<p>For each image, the response mapping will be shown on screen, and you should press the corresponding key to indicate your categorization. If your answer is wrong, \
                              feedback will be provided and the same image will reappear so you can correct your answer.</p>'+
                              '<p>Please press NEXT to start.</p></div>'
                             ];

var famBlockInstruction = [ '<div class = "InstrBx"><p>Now you have seen all the images and know their correct categories.</p>'+
                            
                            '<p>To avoid any possible confusion on the images, we summarized all the categories, which you can use as reference if you are still not sure about certain image(s) or categories.</p>'+

                            '<p>Press NEXT to check the summary.</p></div>',

                            '<p><img src="instruction_young_old.jpg"></img></p>',
                            '<p><img src="instruction_small_big.jpg"></img></p>',
                            '<p><img src="instruction_water_land.jpg"></img></p>',

                            '<p>Now we hope you are more clear about the images and their categories.</p>' +
                            '<p>Press NEXT to categorize the images again to strengthen your memory</p>'
                          ];                                                                                       
/*
var regularBlockInstruction = ['<p><div class = "InstrBx">The next block is a <b>regular</b> block. In this block, there will be not transform trials, so the task cue will NEVER change.</p>' +

                               '<p>the response mapping you are going to use is:</p>' +
                               '<table><tr><th>left key </th><th> right key</th></tr>'+
                                      '<tr><td>'+ leftPrompt[0] +'</td><td>'+ rightPrompt[0] +'</td></tr>'+
                                      '<tr><td>'+ leftPrompt[1] +'</td><td>'+ rightPrompt[1] +'</td></tr>'+
                                      '<tr><td>'+ leftPrompt[2] +'</td><td>'+ rightPrompt[2] +'</td></tr></table>' +

                               '<p>Just to be clear, the response mapping will NOT change throughout the experiment. You can perform the task efficiently by placing your left and right index\
                               fingers on the letters \"F\" and \"J\".</p>' +
                               '<p><img src="instruction_keyboard.jpg"></img></p></div>' +
                               '<p>Please press NEXT to start.</p></div>' 
                              ];


var transformBlockInstruction = ['<p><div class = "InstrBx">The next block is a transform block. In this block, there are some transformed trials, please be prepared for that.</p>' +

                                 '<p>Importantly, to help you better prepare for the transformed trial, you can pay attention to the time interval between the cue and the images, indicated\
                                  by those 3 enlarging fixation crosses. The longer you have to wait following the first task cue, the more likely the task will transform!</p>' +

                                 '<p>the response mapping you are going to use is:</p>' +
                                 '<table><tr><th>left key </th><th> right key</th></tr>'+
                                      '<tr><td>'+ leftPrompt[0] +'</td><td>'+ rightPrompt[0] +'</td></tr>'+
                                      '<tr><td>'+ leftPrompt[1] +'</td><td>'+ rightPrompt[1] +'</td></tr>'+
                                      '<tr><td>'+ leftPrompt[2] +'</td><td>'+ rightPrompt[2] +'</td></tr></table>' +
                                 '<p>Just to be clear, the response mapping will NOT change throughout the experiment. You can perform the task efficiently by placing your left and right\
                                  index fingers on the letters \"F\" and \"J\".</p>' +
                                 '<p><img src="instruction_keyboard.jpg" style = "width:500px;height:200px;"></img></p></div>' + 
                                 '<p>Please press NEXT to start.</p></div>'
                                 ];
*/
var endMessage = ['<p><div class = "InstrBx">Congratulations! You now successfully finished the whole experiment</p>'+ 
                  '<p>Thanks again for your participation!</p>'+
                  '<p>Feel free to contact the experimenter (Mengqiao.Chai@ugent.be) if you want to have more information about this experiment</p>'+
                  '<p>Now you can press NEXT to exit the experiment and you will be directed to Google frontpage</p></div>'];